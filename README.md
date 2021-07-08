Small project to create something of a '1000-points' score-card for climbing competitions.
Everything works but it's more a proof of concept then something to actually be used.

## Features  
* Log a route
* Auto-updating ranking
* Add new routes
* Local state caching such that no wait time is needed for firebase queries.

## Technologies

Written in Elm, powered by firebase, glued together with some javascript.
The firebase part utilizes some firebase cloud functions for more advanced control such as the auto updating ranking and score calculation.

Some Bulma is used to create a responsive layout.

## Using this
You probably wont. But if you would, you should create your own firebase project.