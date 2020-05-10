type node;

let div:
  (
    ~key: Brisk_reconciler.Key.t=?,
    ~children: Brisk_reconciler.element(node),
    ~id: string=?,
    ~className: string=?,
    unit
  ) =>
  Brisk_reconciler.element(node);

let span:
  (
    ~key: Brisk_reconciler.Key.t=?,
    ~className: string=?,
    ~text: string=?,
    ~onClick: unit => unit=?,
    unit
  ) =>
  Brisk_reconciler.element(node);

let img:
  (~key: Brisk_reconciler.Key.t=?, ~className: string, ~src: string, unit) =>
  Brisk_reconciler.element(node);

let input:
  (~key: Brisk_reconciler.Key.t=?, ~className: string,  ~value: string, ~onChange: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.event) => unit, unit) =>
  Brisk_reconciler.element(node);

let render: Brisk_reconciler.element(node) => unit;
let renderToString: Brisk_reconciler.element(node) => string;
