import Link from "next/link";
import React from "react";
import Layout from "../components/layout";
import bg from '../../public/background.jpg';
import Image from 'next/image';

export default function IndexPage() {
    return (
        <Layout title={"Home"}>
            <div className="mx-auto">
                <BackgroundImage/>
            </div>
        </Layout>
    );
}

const BackgroundImage = () => {
    return (
        <div className="w-full h-full opacity-80">
            <img src='/background.jpg'/>
        </div>
    )
}

