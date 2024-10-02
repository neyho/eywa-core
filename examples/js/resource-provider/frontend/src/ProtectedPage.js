import React from 'react';

function ProtectedPage() {
  return (
    <div>
      <h2>Protected Page</h2>
      <p>This is a protected page, accessible only with a valid OIDC token.</p>
    </div>
  );
}

export default ProtectedPage;

