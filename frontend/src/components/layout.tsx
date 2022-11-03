import Head from "next/head";
import React from "react";
import Navbar from "./navbar";

type Props = {
    children: React.ReactNode;
    title: string;
};

function Layout({ children, title }: Props) {
    return (
        <React.Fragment>
            <Head>
                <title>{title} | P7 Haskell Runner</title>
                <meta name="description" content="Haskell Runner." />
                <link rel="icon" type="image/x-icon" href="favicon.ico" />
            </Head>
            <div className="h-screen flex flex-col bg-slate-900">
                <Navbar />
                {children}
            </div>
        </React.Fragment>
    );
}

export default Layout;
