import React from 'react';
import { Link, Routes, Route } from 'react-router-dom';
import Home from './Home';
import About from './About';
import ClickCounter from './ClickCounter';
import { useAuth} from 'react-oidc-context';

function App() {
  const auth = useAuth()
  console.log(auth)
  return (
    <div>
      <nav>
        <ul>
          <li>
            <Link to="/">Home</Link>
          </li>
          <li>
            <Link to="/about">About</Link>
          </li>
          {auth.isAuthenticated? (
            <>
              <li>
                <button onClick={() => auth.signoutSilent()}>Logout</button>
              </li>
              <div>
                <p>Welcome, {auth.user?.profile.sub}</p>
              </div>
              </>
          ) : (
            <li>
                <button onClick={() => {
                  console.log(auth)
                  auth.signinRedirect();
                  }
                }>Log in</button>
            </li>
          )}
          <ClickCounter />
        </ul>
      </nav>

      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/about" element={<About />} />
      </Routes>
    </div>
  );
}

export default App;

