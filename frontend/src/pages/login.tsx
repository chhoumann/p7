import { NextPage } from 'next';
import { useSession, signIn, signOut } from 'next-auth/react';
import React from 'react'

const Testpage: NextPage = () => {

  const {data: session} = useSession();

  if (session) {
    return (
        <>
            Signed in as {session?.user?.email} <br />
            <button onClick={() => signOut()}>Sign out</button>
        </>
    )
  }

  return (
    <>
        Not signed in <br />
        <button onClick={() => signIn()}>Sign in</button>
    </>
  )

}

export default Testpage