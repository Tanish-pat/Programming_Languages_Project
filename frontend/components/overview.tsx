"use client"

import { Bar, BarChart, ResponsiveContainer, XAxis, YAxis } from "recharts"

const data = [
  {
    name: "Jan",
    total: 1200,
  },
  {
    name: "Feb",
    total: 1900,
  },
  {
    name: "Mar",
    total: 2300,
  },
  {
    name: "Apr",
    total: 2800,
  },
  {
    name: "May",
    total: 3500,
  },
  {
    name: "Jun",
    total: 3000,
  },
  {
    name: "Jul",
    total: 2500,
  },
  {
    name: "Aug",
    total: 3800,
  },
  {
    name: "Sep",
    total: 4200,
  },
  {
    name: "Oct",
    total: 4000,
  },
  {
    name: "Nov",
    total: 4500,
  },
  {
    name: "Dec",
    total: 5200,
  },
]

export function Overview() {
  return (
    <ResponsiveContainer width="100%" height={350}>
      <BarChart data={data}>
        <XAxis dataKey="name" stroke="#888888" fontSize={12} tickLine={false} axisLine={false} />
        <YAxis
          stroke="#888888"
          fontSize={12}
          tickLine={false}
          axisLine={false}
          tickFormatter={(value) => `$${value}`}
        />
        <Bar dataKey="total" fill="currentColor" radius={[4, 4, 0, 0]} className="fill-primary" />
      </BarChart>
    </ResponsiveContainer>
  )
}
