const functions = require("firebase-functions");
const admin = require('firebase-admin');
const { user } = require("firebase-functions/lib/providers/auth");
admin.initializeApp();

const db = admin.firestore();
exports.aggregateRoutes = functions.firestore
    .document('routes/{routeId}/logs/{logId}')
    .onWrite(async (change, context) => {
        const routeRef = db.collection('routes').doc(context.params.routeId);
        const logsCollection = routeRef.collection('logs');
        const query = logsCollection.where('style', '!=', "none");

        return db.runTransaction(async (transaction) => {
            return transaction.get(query).then(query => {
                return transaction.update(routeRef, { logCount: query.size })
            })
        });
    });


exports.updateRanking = functions.pubsub.schedule("every 30 minutes").onRun(async (context) => {
    const ranking = []

    const users = await db.collection('users').listDocuments();
    return Promise.all(users.map(async user => {
        let climbed = 0;
        let points = 0;
        const logs = await db.collectionGroup('logs').where('uid', '==', user.id).get();
        return Promise.all(logs.docs.map(async log => {
            if (log.data().style !== "none") {
                climbed += 1;
                const route = await log.ref.parent.parent.get();
                points += route.data().logCount === 0 ? route.data().points : route.data().points / route.data().logCount
            }

        })).then(_ =>
            ranking.push({ climbed: climbed, points: points, userRef: user })
        );

    })).then(_ => {
        ranking.sort((a, b) => b.points - a.points || a.climbed - b.climbed);
        return db.runTransaction(async (transaction) => {
            return Promise.all(ranking.map((rank, i) => {
                return transaction.set(rank.userRef, { climbed: rank.climbed, points: rank.points, position: i }, { merge: true });
            }));
        })
    });
});

exports.ranking = functions.https.onRequest(async (request, response) => {
    const ranking = []

    const users = await db.collection('users').listDocuments();
    await Promise.all(users.map(async user => {
        let climbed = 0;
        let points = 0;
        const logs = await db.collectionGroup('logs').where('uid', '==', user.id).get();
        await Promise.all(logs.docs.map(async log => {
            if (log.data().style !== "none") {
                climbed += 1;
                const route = await log.ref.parent.parent.get();
                points += route.data().logCount === 0 ? route.data().points : route.data().points / route.data().logCount
            }

        })).then(_ =>
            ranking.push({ climbed: climbed, points: points })
        );

    })).then(_ =>
        response.send(ranking.sort((a, b) => b.points - a.points || a.climbed - b.climbed))
    );
});

