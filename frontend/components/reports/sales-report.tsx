"use client"

import { Bar, BarChart, CartesianGrid, Legend, ResponsiveContainer, Tooltip, XAxis, YAxis } from "recharts"

const data = [
  {
    name: "Jan",
    sales: 4000,
    profit: 2400,
  },
  {
    name: "Feb",
    sales: 3000,
    profit: 1398,
  },
  {
    name: "Mar",
    sales: 9800,
    profit: 2000,
  },
  {
    name: "Apr",
    sales: 3908,
    profit: 2780,
  },
  {
    name: "May",
    sales: 4800,
    profit: 1890,
  },
  {
    name: "Jun",
    sales: 3800,
    profit: 2390,
  },
  {
    name: "Jul",
    sales: 4300,
    profit: 3490,
  },
]

export function SalesReport() {
  return (
    <ResponsiveContainer width="100%" height={400}>
      <BarChart data={data}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="name" />
        <YAxis />
        <Tooltip formatter={(value) => `$${value}`} />
        <Legend />
        <Bar dataKey="sales" name="Total Sales" fill="#3b82f6" />
        <Bar dataKey="profit" name="Profit" fill="#10b981" />
      </BarChart>
    </ResponsiveContainer>
  )
}
