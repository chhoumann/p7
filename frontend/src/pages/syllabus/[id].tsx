import { NextPage } from 'next';
import { useRouter } from 'next/router'
import { trpc } from "../../utils/trpc"
import React, { useState } from 'react'


const SyllabusId: NextPage = () => {

    const [selectedId, setSelectedId] = useState<string>()

    const router = useRouter();
    const { id } = router.query;
    //const sessions = trpc.useQuery
    const testData = [
        { name: "hello", id: "i21en" },
        { name: "Jesus", id: "i23en" },
        { name: "Christ", id: "i22en" },
    ]


    return (
        <div className='container flex justify-center items-center w-full h-[80vh]'>
            <div className='flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500'>

                {testData.map((session) =>
                    <React.Fragment key={session.id}>
                        <SessionRow {...session} onClick={() => setSelectedId(session.id)} isSelected={session.id === selectedId}/>
                    </React.Fragment>
                )}
                <div className='mb-auto' />
                <div className='flex flex-row gap-4 mx-3 my-3'>
                    <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                        Create new session
                    </button>
                    <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                        Edit
                    </button>
                </div>
            </div>
        </div>
    )
}

export default SyllabusId;


function SessionRow({ id, name, isSelected, onClick }:{ id: string, name: string, isSelected: boolean, onClick: () => void }) {


    return (
        <div className={`p-3 m-3 ${isSelected?'bg-gray-400':"bg-gray-300 hover:bg-gray-400"}`} onClick={onClick}>
            <span>{name}</span>
        </div>
    )
}