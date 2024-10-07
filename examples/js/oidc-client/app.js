// app.js

const express = require('express');
const session = require('express-session');
const axios = require('axios');
const { Issuer } = require('openid-client');
const path = require('path');

const app = express();
const port = 3000;

// Express session setup
app.use(
  session({
    secret: 'super-secret-key',
    resave: false,
    saveUninitialized: true,
    cookie: { secure: false },
  })
);

let client;

// Discover OIDC provider information and initialize the client
Issuer.discover('http://localhost:8080') // Replace with your OIDC provider URL
  .then((oidcIssuer) => {
    console.log('Discovered issuer %s', oidcIssuer.issuer);

    // Initialize the client
    client = new oidcIssuer.Client({
      client_id: 'GMTXXCLYXPIJHCLYHPVZDWWARYPMVLGIKFXKFAZNIGKLIDNJ',         // Replace with your client ID
      client_secret: '-55tzQoNsHYcKqxPi9SCNvLDi9eYJgMl7n3vVAsy5uXhZrFx', // Replace with your client secret
      redirect_uris: ['http://localhost:3000/callback'], // OIDC callback URL
      post_logout_redirect_uris: ['http://localhost:3000'],
      response_types: ['code'],
      automaticSilentRenew: true,
      silent_redirect_uri: 'http://localhost:3000/silent-renew',
    });
  })
  .catch((err) => {
    console.error('Error during OIDC discovery:', err);
  });


app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.html'));  // Serve the HTML file with buttons
});

// Route to start the OIDC login process
app.get('/login', (req, res) => {
  const state = Math.random().toString(36).substring(7)
  req.session.state = state;
  const authorizationUrl = client.authorizationUrl({
    scope: 'openid profile email roles permissions offline_access', // Define your OIDC scopes
  });
  res.redirect(authorizationUrl);
});

// OIDC callback route to handle the authorization code
app.get('/callback', async (req, res) => {
  const params = client.callbackParams(req);

  try {
    const tokenSet = await client.callback('http://localhost:3000/callback', params);
    req.session.tokenSet = tokenSet; // Store tokens in session

    res.send(`<h1>Logged in</h1><pre>${JSON.stringify(tokenSet, null, 2)}</pre>`);
  } catch (err) {
    res.status(500).send('Error processing callback');
    console.error(err);
  }
});

// Route to access a protected resource
app.get('/profile', async (req, res) => {
  if (!req.session.tokenSet) {
    return res.redirect('/login');
  }

  try {
    const userinfo = await client.userinfo(req.session.tokenSet.access_token);
    res.send(`<h1>User Profile</h1><pre>${JSON.stringify(req.session.tokenSet, null, 2)}</pre>`);
  } catch (err) {
    res.status(500).send('Error fetching profile');
    console.error(err);
  }
});



app.get('/logout', (req, res) => {
  if (!req.session.tokenSet) {
    return res.redirect('/');
  }

  // Clear the session
  req.session.destroy();

  // Get the end_session_endpoint if available from the OIDC provider
  const endSessionUrl = client.endSessionUrl({
    id_token_hint: req.session.tokenSet.id_token, // Provide the ID token for proper logout at the provider
    post_logout_redirect_uri: 'http://localhost:3000', // Redirect back after logout
  });

  // Redirect the user to the OIDC provider's logout endpoint
  res.redirect(endSessionUrl);
});


function ensureAuthenticated(req, res, next) {
  if (req.session && req.session.tokenSet.access_token) {
    // If the access token is available in the session, the user is authenticated
    return next();
  }
  
  // If not authenticated, redirect to login or send an unauthorized response
  res.redirect('/auth/login');
}


// Middleware to forward requests while keeping the access token
app.use('/list-users', ensureAuthenticated, async (req, res) => {
  try {
    // Get the access token from the authenticated user
    const accessToken = req.session.tokenSet.access_token;

    // Forward the request to a different endpoint
    const response = await axios({
      method: "POST",
      url: 'http://localhost:8080/graphql', // Target endpoint
      headers: {
        'Authorization': `Bearer ${accessToken}`, // Attach access token
        'Content-Type': 'application/json',
      },
      data: JSON.stringify(
      {query: `{
                 searchUser {
                    euuid
                    name
                    settings
                    avatar
                 }
      }`})});

    // Send the response back to the client
    res.status(response.status).json(response.data);
  } catch (error) {
    console.error('Error forwarding request:', error);
    res.status(error.response?.status || 500).json({
      message: 'Error forwarding request',
    });
  }
});



app.listen(port, () => {
  console.log(`OIDC app listening at http://localhost:${port}`);
});
