# SnapChat

Example application for the Snap framework. The backend (written using Snap and Haskell) keeps a list of users and posts. The frontend uses jQuery and Javascript to maintain a
connection to the backend and post data.

Note that the server employs multiple threads. To avoid threading errors, SnapChat uses Haskell\'s excellent STM library.  
