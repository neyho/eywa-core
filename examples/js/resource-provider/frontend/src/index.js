import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import { AuthProvider } from "react-oidc-context";
import { BrowserRouter as Router, Route, Routes} from 'react-router-dom';
import {Log} from 'oidc-client-ts';


Log.setLevel(Log.DEBUG)
Log.setLogger(console)

// OIDC Configuration
const oidcConfiguration = {
  client_id: 'GMTXXCLYXPIJHCLYHPVZDWWARYPMVLGIKFXKFAZNIGKLIDNJ',
  client_secret: '-55tzQoNsHYcKqxPi9SCNvLDi9eYJgMl7n3vVAsy5uXhZrFx',
  redirect_uri: 'http://localhost:3000/auth-callback',
  authority: 'http://localhost:8080',
  response_type: 'code',
  scope: 'openid profile email sub:uuid roles permissions',
  silent_redirect_uri: 'http://localhost:3000/silent-renew',
  post_logout_redirect_uri: "http://localhost:3000/",
  automaticSilentRenew: true,
  loadUserInfo: true,
  onSigninCallback: () => window.history.pushState(null,"","/")
};


const rootElement = document.getElementById('root');
const root = ReactDOM.createRoot(rootElement);

console.log(AuthProvider)


root.render(
  <React.StrictMode>
    <AuthProvider {...oidcConfiguration}>
      <Router>
        <App/>
      </Router>
    </AuthProvider>
  </React.StrictMode>
);
