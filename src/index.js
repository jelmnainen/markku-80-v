import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
const progressKey = 'markun_matka_progress'
const progression = window.localStorage.getItem(progressKey) ? window.localStorage.getItem(progressKey) : JSON.stringify([])

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    contentfulSpaceId: process.env.ELM_APP_CONTENTUFL_SPACE_ID,
    contentfulAccessKey: process.env.ELM_APP_CONTENTFUL_SPACE_ACCESS_KEY,
    progression,
  }
});


// Ports
app.ports.saveProgressToLocalStore.subscribe((msg) => {
  window.localStorage.setItem(progressKey, msg)
})


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
