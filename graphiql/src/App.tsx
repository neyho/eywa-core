import React from 'react';
import { GraphiQL } from 'graphiql';
import type { Fetcher } from '@graphiql/toolkit';
import 'graphiql/graphiql.min.css';

console.log(window.location)

const fetcher: Fetcher = async graphQLParams => {
  let token = window.localStorage.getItem('eywa.token');
  let origin = window.location.origin
  console.log(`Token: ${token}`) 
  const data = await fetch(
    `${origin}/graphql`,
    {
      method: 'POST',
      headers: {
        Accept: 'application/json',
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify(graphQLParams)
    },
  );
  return data.json().catch(() => data.text());
};

const App = () => <GraphiQL fetcher={fetcher} />;

export default App;
