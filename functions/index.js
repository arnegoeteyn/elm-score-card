const functions = require("firebase-functions");
const admin = require('firebase-admin');
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

