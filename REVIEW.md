# 移流方程式ソルバー 実装レビュー

## 概要

このドキュメントは、Toro (2024) "Computational algorithms for shallow water equations" に基づいた1次元移流方程式（Linear Advection Equation, LAE）ソルバーの実装をレビューし、保守性および拡張性の観点から問題点と改善提案をまとめたものです。

---

## 1. アーキテクチャの概観

### 現在の構成

```
fortran/
├── app/                          # アプリケーション（エントリポイント）
│   ├── lae_1d_riemann_exact_calculator.f90
│   ├── lae_1d_riemann_numerical_calculator.f90
│   └── mesh_1d_creator.f90
└── src/
    ├── io/                       # 入出力
    │   ├── class_mesh_1d_reader.f90
    │   └── class_text_writer.f90
    ├── lae/                      # LAEソルバー関連
    │   ├── abstract_initial_condition_1d.f90
    │   ├── abstract_lae_1d_solver.f90
    │   ├── class_config_lae_1d.f90
    │   ├── class_lae_1d_solution.f90
    │   ├── class_lae_1d_solver_ftcs.f90
    │   ├── class_lae_1d_solver_godunov.f90
    │   ├── class_lae_1d_solver_lax_friedrichs.f90
    │   └── class_riemann_1d_solution.f90
    └── mesh/                     # メッシュ関連
        ├── class_mesh_1d.f90
        └── class_mesh_1d_creator.f90
```

---

## 2. 主要な問題点

### 2.1 コードの重複（DRY原則違反）

#### 問題

3つのソルバークラス（`t_lae_1d_solver_ftcs`、`t_lae_1d_solver_godunov`、`t_lae_1d_solver_lax_friedrichs`）の間でほぼ同一のコードが繰り返されています。

**重複している内容：**
- 型定義のメンバ変数（`solution`, `dt`, `dx`, `lambda`, `c`, `n_nodes`, `bl`, `br` など）
- `initialize`、`set_initial_condition`、`set_boundary_condition` サブルーチン
- `evaluate_next` 関数の前処理部分（状態チェック、配列確保）

#### 影響

- バグ修正や機能追加の際に、3ファイル全てを修正する必要がある
- 修正漏れによる不整合が発生しやすい
- コード量が不必要に増大

#### 改善提案

```fortran
! 基底クラスを拡張して共通機能を集約
type, abstract, extends(t_abstract_lae_1d_solver) :: t_base_lae_1d_solver
    ! 共通のメンバ変数をここに定義
    real(real64), allocatable :: solution(:)
    real(real64) :: dt, dx, lambda, c
    ! ...
    contains
    procedure :: initialize        ! 共通の初期化処理
    procedure :: set_initial_condition
    procedure :: set_boundary_condition
    procedure(compute_flux_interface), deferred :: compute_flux  ! 各スキーム固有
end type
```

---

### 2.2 抽象クラスのインターフェース設計の問題

#### 問題

`t_abstract_lae_1d_solver` が `evaluate_next` メソッドのみを定義していますが、実際の実装クラスでは `initialize`、`set_initial_condition`、`set_boundary_condition` といったメソッドも必要です。これらが抽象インターフェースに含まれていないため、ポリモーフィズムを活用したコードが書けません。

#### 影響

- ソルバーの切り替えを実現するためのファクトリパターンが実装しにくい
- 設定ファイルからソルバーを動的に選択する機能の実装が困難

#### 改善提案

```fortran
type, abstract :: t_abstract_lae_1d_solver
    contains
    procedure(initialize_interface), deferred :: initialize
    procedure(set_initial_condition_interface), deferred :: set_initial_condition
    procedure(set_boundary_condition_interface), deferred :: set_boundary_condition
    procedure(evaluate_next_interface), deferred :: evaluate_next
end type
```

---

### 2.3 型の不整合

#### 問題

`n_nodes` が `real(real64)` として定義されていますが、ノード数は整数であるべきです。

**該当箇所：**
- `class_lae_1d_solver_ftcs.f90` 31行目
- `class_lae_1d_solver_godunov.f90` 29行目
- `class_lae_1d_solver_lax_friedrichs.f90` 31行目

#### 影響

- 暗黙の型変換による精度損失のリスク
- コードの意図が不明確になる

#### 改善提案

```fortran
! 変更前
real(real64) :: n_nodes

! 変更後
integer(int32) :: n_nodes
```

---

### 2.4 変数名の命名規則の不整合

#### 問題

境界条件の変数名が逆になっている可能性があります。

```fortran
!> Left oundary value   ← コメントには Left と記載
real(real64) :: br      ← 変数名は br（right の略？）

!> Right boundary value ← コメントには Right と記載
real(real64) :: bl      ← 変数名は bl（left の略？）
```

#### 影響

- コードの可読性低下
- バグ発生時の原因特定が困難

#### 改善提案

コメントと変数名を一致させる、または `boundary_left`、`boundary_right` のような明確な名前に変更します。

---

### 2.5 エラーチェックのロジック誤り

#### 問題

`evaluate_next` 関数内で、境界条件のチェックに誤ったフラグを使用しています。

**該当箇所（3ファイル共通）：**

```fortran
if(.not. this%has_initial_condition) then
    error stop class_name // ".evaluate_next: Boundary condition is not set."
end if
```

`has_initial_condition` を2回チェックしており、`has_boundary_condition` のチェックがありません。

#### 改善提案

```fortran
if(.not. this%has_boundary_condition) then
    error stop class_name // ".evaluate_next: Boundary condition is not set."
end if
```

---

### 2.6 設定ファイルのオプション読み込み未完成

#### 問題

`class_config_lae_1d.f90` の `load_option` サブルーチンで `algorithm` パラメータを読み込んでいますが、読み込んだ値を保持・利用していません。

```fortran
subroutine load_option(this, json)
    ! ...
    call json%get("numerical.option.algorithm", algorithm)
    ! algorithm 変数が使われていない
end subroutine
```

#### 影響

- 設定ファイルで異なるアルゴリズムを指定しても切り替わらない
- アプリケーション側で特定のソルバークラスがハードコードされている

#### 改善提案

1. `t_numerical` 型に `algorithm` フィールドを追加
2. ファクトリパターンを導入してソルバーを動的に生成

```fortran
type :: t_numerical
    ! ...
    character(:), allocatable :: algorithm  ! "ftcs", "godunov", "lax_friedrichs"
end type

! ファクトリ関数
function create_solver(algorithm) result(solver)
    character(*), intent(in) :: algorithm
    class(t_abstract_lae_1d_solver), allocatable :: solver
    
    select case(algorithm)
    case("ftcs")
        allocate(t_lae_1d_solver_ftcs :: solver)
    case("godunov")
        allocate(t_lae_1d_solver_godunov :: solver)
    case("lax_friedrichs")
        allocate(t_lae_1d_solver_lax_friedrichs :: solver)
    end select
end function
```

---

### 2.7 テストの欠如

#### 問題

単体テストやインテグレーションテストが存在しません。

#### 影響

- リファクタリング時に退行バグを検出できない
- ソルバーの精度検証が手動になる

#### 改善提案

fpmを使用したテストインフラを構築し、以下のテストを追加することを推奨します：

- 各ソルバーの基本動作テスト
- 解析解との比較テスト（Riemann問題など）
- 境界条件の検証テスト

---

## 3. 拡張性に関する考慮事項

### 3.1 新しい数値スキームの追加

**現状の問題：**
- 新しいソルバーを追加する際、大量のボイラープレートコードのコピーが必要
- アプリケーション側でソルバーがハードコードされているため、新規ソルバーの利用には修正が必要

**推奨される改善：**
1. 共通機能を持つ基底クラスの導入
2. ファクトリパターンによるソルバーの動的生成
3. 設定ファイルベースでのソルバー選択機能

### 3.2 多次元への拡張

**現状の問題：**
- クラス名・変数名に `1d` が含まれているが、設計自体が1次元に限定されていない
- メッシュ構造が配列ベースで柔軟性が低い

**推奨される改善：**
1. 次元に依存しない抽象メッシュ型の導入
2. 構造格子・非構造格子の両方に対応できるインターフェース設計

### 3.3 境界条件の柔軟性

**現状の問題：**
- 固定値（ディリクレ）境界条件のみ対応
- 周期境界条件やノイマン境界条件に対応できない

**推奨される改善：**
```fortran
type, abstract :: t_boundary_condition
    contains
    procedure(apply_interface), deferred :: apply
end type

type, extends(t_boundary_condition) :: t_dirichlet_bc
    real(real64) :: value
end type

type, extends(t_boundary_condition) :: t_periodic_bc
end type

type, extends(t_boundary_condition) :: t_neumann_bc
    real(real64) :: gradient
end type
```

---

## 4. Pythonビジュアライザについて

### 4.1 良い点

- コンテキストマネージャ（`with`文）のサポート
- 適切な型ヒントの使用
- エラーメッセージが具体的

### 4.2 改善点

- `except:` の使用（ベアexcept）は避け、特定の例外を捕捉すべき
  ```python
  # 変更前
  except:
      raise ValueError(...)
  
  # 変更後
  except (ValueError, IndexError) as e:
      raise ValueError(...) from e
  ```

- `solution_list` パラメータの機能が未完成（読み込むが使用されていない）

---

## 5. 優先度別改善計画

### 高優先度（バグ修正）

1. 境界条件チェックのロジック修正（`has_boundary_condition` の使用）
2. `n_nodes` の型を `integer(int32)` に修正
3. 境界値変数名（`bl`/`br`）とコメントの整合性確認

### 中優先度（保守性向上）

1. ソルバー基底クラスの導入によるコード重複の解消
2. ファクトリパターンの導入
3. 抽象インターフェースの拡充

### 低優先度（拡張性向上）

1. テストインフラの構築
2. 境界条件の抽象化
3. 多次元対応の準備

---

## 6. まとめ

本実装はプロトタイプとして機能しますが、以下の点で改善が必要です：

1. **コードの重複**：3つのソルバークラス間で大量の重複コードがあり、保守性を大きく低下させています
2. **抽象化の不足**：ポリモーフィズムを活用するための設計が不十分です
3. **軽微なバグ**：境界条件チェックのロジック誤り、型の不整合が存在します
4. **拡張性**：新しい数値スキームや境界条件を追加する際に大幅な修正が必要になります

上記の改善を行うことで、より保守しやすく、拡張性の高いコードベースになることが期待されます。
