import Link from "next/link";
import React from "react";
import Layout from "../components/layout";
import SolveProblem from "../pages/problem/[problemId]/index";

export default function IndexPage() {
    return (
        <Layout title={"Home"}>
            <div className="background-content">
                <h3 className="landing-title-first">Welcome to</h3>
                <h3 className="landing-title-second">The AAU Programming</h3>
                <h3 className="landing-title-third">Paradigms Course</h3>
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

