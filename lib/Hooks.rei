open Brisk_reconciler.Hooks;

let state:
  ('a, t(State.t('a) => 'b, 'c)) => (('a, ('a => 'a) => unit), t('b, 'c));

let reducer:
  (~initialState: 'a, ('b, 'a) => 'a, t(Reducer.t('a) => 'c, 'd)) =>
  (('a, 'b => unit), t('c, 'd));

let effect:
  (
    Effect.condition('a),
    unit => option(unit => unit),
    t(Effect.t('a) => 'b, 'c)
  ) =>
  (unit, t('b, 'c));
