import React from "react";


function Navbar() {
    return (
        <nav>
            <div className="container flex flex-wrap justify-between items-center border-b-2 p-4">
                <span className="font-bold link link-underline link-underline-black">Home</span>
                <span className="font-bold rounded-r-2xl rounded-l-2xl py-1 px-4 border-2 border-solid border-cyan-900 hover:border-gray-800">Login</span>
            </div>
        </nav>
    );
}

export default Navbar;
