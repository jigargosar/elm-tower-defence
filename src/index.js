const Module = require('./Main.elm')
require('./styles.css')
require('tachyons')
const { pathOr, identity } = require('ramda')

{
  const app = initElmApp()
  const fss = pathOr(identity, ['ports', 'focusSelector', 'subscribe'])(
    app,
  )
  fss(function(selector) {
    requestAnimationFrame(function() {
      const el = document.querySelector(selector)
      if (el) {
        el.focus()
      } else {
        console.error('Focus Error ', selector)
      }
    })
  })
}

function initElmApp() {
  const app = Module.Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      now: Date.now(),
      viewSize: [window.innerWidth, window.innerHeight],
      scrollbarSize: [
        window.innerWidth - document.body.clientWidth,
        window.innerHeight - document.body.clientHeight,
      ],
    },
  })
  return app
}


