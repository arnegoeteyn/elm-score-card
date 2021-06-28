import "./main.css";
import * as firebase from "firebase/app";
import "firebase/firebase-auth";
import "firebase/firebase-firestore";

import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const unsubscribers = {}

function unsubscribeAll() {
  console.log(Object.entries(unsubscribers))
  for (const [_, value] of Object.entries(unsubscribers)) {
    console.log(value);
    if (value !== null) {
      value();
    }
  }
}

function setUpLogsSnapshot(user, routes) {
  if (unsubscribers.userRoutesUnsubscribe) {
    unsubscribers.userRoutesUnsubscribe();
  }

  unsubscribers.userRoutesUnsubscribe = db.collectionGroup('logs').where('uid', '==', user.uid).onSnapshot(docs => {
    if (routes) {
      docs.forEach(doc => {
        const routeId = doc.ref.parent.parent.id
        routes[routeId].style = doc.data().style;
      }, error => console.log("Oei"));

      app.ports.receiveRoutes.send({
        routes: Object.values(routes)
      });

      routes = null;
    } else {
      console.log("Received new personal routes");
      docs.forEach(doc => {
        const routeId = doc.ref.parent.parent.id
        console.log("routeupdate" + routeId)
        app.ports.receiveRouteUpdate.send({
          routeId: routeId,
          style: doc.data().style
        });
      }, error => console.log("Oei"));
    }
  })
}


function setUpRoutesSnapShot(user) {
  console.log(user.uid)

  unsubscribers.userRoutesUnsubscribe = null;

  unsubscribers.routesUnsubscribe = db.collection(`routes`).onSnapshot(docs => {
    console.log("Received new routes");
    const routesMap = {}

    docs.forEach(doc => {
      if (doc.data()) {
        routesMap[doc.id] = { ...doc.data(), ...{ 'style': "none", 'id': doc.id } }
      }
    });

    // app.ports.receiveRoutes.send({
    //   routes: Object.values(routesMap)
    // });

    setUpLogsSnapshot(user, routesMap);

  })

}

// Just checking envs are defined - Debug statement
console.log(process.env.ELM_APP_API_KEY !== undefined);

const firebaseConfig = {
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  databaseURL: process.env.ELM_APP_DATABASE_URL,
  projectId: process.env.ELM_APP_PROJECT_ID,
  storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
  messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
  appId: process.env.ELM_APP_APP_ID
};

firebase.initializeApp(firebaseConfig);

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();

const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.signIn.subscribe(() => {
  console.log("LogIn called");
  firebase
    .auth()
    .signInWithPopup(provider)
    .then(result => {
      result.user.getIdToken().then(idToken => {
        app.ports.signInInfo.send({
          token: idToken,
          email: result.user.email,
          uid: result.user.uid
        });
      });
    })
    .catch(error => {
      app.ports.signInError.send({
        code: error.code,
        message: error.message
      });
    });
});

app.ports.signOut.subscribe(() => {
  console.log("LogOut called");

  app.ports.receiveRoutes.send({
    routes: []
  });

  unsubscribeAll();

  firebase.auth().signOut().then(succes => console.log("signed out"), error => console.log("something wrong with logout"));
});

//  Observer on user info
firebase.auth().onAuthStateChanged(user => {
  console.log("called");
  if (user) {
    console.log("user")
    user
      .getIdToken()
      .then(idToken => {
        app.ports.signInInfo.send({
          token: idToken,
          email: user.email,
          uid: user.uid
        });
      })
      .catch(error => {
        console.log("Error when retrieving cached user");
        console.log(error);
      });

    setUpRoutesSnapShot(user);
  }
});


app.ports.updateRouteLog.subscribe(data => {
  console.log(`logging route to database : ${data}`);

  db.collection(`routes/${data.routeId}/logs`)
    .doc(data.uid).set(
      { style: data.style, uid: data.uid }
    );
});


app.ports.updateRoute.subscribe(data => {
  console.log(`new route : ${data.route.name}`)
  let collection = db.collection('routes')
  let doc = null;
  let notificationText = null;
  if (data.routeId) {
    doc = collection.doc(data.routeId);
    notificationText = "Route updated"
  } else {
    doc = collection.doc();
    notificationText = "New route created"
  }
  doc.set(data.route).then(succes => {
    app.ports.receiveNotification.send(
      notificationText
    )
  });
})

registerServiceWorker();
