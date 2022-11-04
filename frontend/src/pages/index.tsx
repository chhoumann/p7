import Link from "next/link";
import React from "react";
import Layout from "../components/layout";
import SolveProblem from "../pages/problem/[problemId]/index";

export default function IndexPage() {
    return (
        <Layout title={"Home"}>
            <div className="background-content">
                <h3 className="landing-title-first max-md">Welcome to</h3>
                <h3 className="landing-title-second max-md">The AAU Programming</h3>
                <h3 className="landing-title-third max-md">Paradigms Course</h3>
            </div>
        </Layout>
    );
}

