import React, { useState } from 'react';
import { useAuth } from 'react-oidc-context';

function ClickCounter() {
  const auth = useAuth();
  const [clickCount, setClickCount] = useState(null);
  const [error, setError] = useState(null);

  const fetchClickCount = async () => {
    try {
      const response = await fetch('http://localhost:4000/count-clicks', {
        method: 'GET',
        headers: {
          'Authorization': `Bearer ${auth.user?.access_token}`, // Attach the access token to the Authorization header
        },
      });

      if (!response.ok) {
        throw new Error('Failed to fetch click count');
      }

      const data = await response.text(); // Get response as plain text
      setClickCount(data);
    } catch (err) {
      setError(err.message);
    }
  };

  return (
    <div>
      <h2>Click Counter</h2>
      <button onClick={fetchClickCount}>Fetch Click Count</button>
      {clickCount && <p>{clickCount}</p>}
      {error && <p style={{ color: 'red' }}>{error}</p>}
    </div>
  );
}

export default ClickCounter;
