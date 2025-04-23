"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Plus, Edit, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { Badge } from "@/components/ui/badge"
import { PaymentAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"
import type { ColumnDef } from "@tanstack/react-table"

interface Payment {
  paymentId: number
  amount: number
  method: string
  status: string
}

export default function PaymentsPage() {
  const { toast } = useToast()
  const [payments, setPayments] = useState<Payment[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentPayment, setCurrentPayment] = useState<Payment | null>(null)

  useEffect(() => {
    fetchPayments()
  }, [])

  const fetchPayments = async () => {
    setIsLoading(true)
    try {
      const data = await PaymentAPI.getAll()
      setPayments(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch payments",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const handleAddNew = () => {
    setCurrentPayment({
      paymentId: 0,
      amount: 0,
      method: "Credit Card",
      status: "Pending",
    })
    setIsDialogOpen(true)
  }

  const handleEdit = (payment: Payment) => {
    setCurrentPayment(payment)
    setIsDialogOpen(true)
  }

  const handleDelete = (payment: Payment) => {
    setCurrentPayment(payment)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentPayment) return

    try {
      await PaymentAPI.delete(currentPayment.paymentId)
      toast({
        title: "Success",
        description: "Payment deleted successfully",
      })
      fetchPayments()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete payment",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value, type } = e.target

    if (!currentPayment) return

    setCurrentPayment({
      ...currentPayment,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSelectChange = (name: string, value: string) => {
    if (!currentPayment) return

    setCurrentPayment({
      ...currentPayment,
      [name]: value,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentPayment) return

    try {
      if (currentPayment.paymentId === 0) {
        await PaymentAPI.create(currentPayment)
        toast({
          title: "Success",
          description: "Payment created successfully",
        })
      } else {
        await PaymentAPI.update(currentPayment.paymentId, currentPayment)
        toast({
          title: "Success",
          description: "Payment updated successfully",
        })
      }
      setIsDialogOpen(false)
      fetchPayments()
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${currentPayment.paymentId === 0 ? "create" : "update"} payment`,
        variant: "destructive",
      })
    }
  }

  const getStatusBadgeVariant = (status: string) => {
    switch (status.toLowerCase()) {
      case "completed":
        return "success"
      case "pending":
        return "warning"
      case "failed":
        return "destructive"
      default:
        return "secondary"
    }
  }

  const columns: ColumnDef<Payment>[] = [
    {
      accessorKey: "paymentId",
      header: "ID",
    },
    {
      accessorKey: "amount",
      header: "Amount",
      cell: ({ row }) => `$${row.original.amount.toFixed(2)}`,
    },
    {
      accessorKey: "method",
      header: "Method",
    },
    {
      accessorKey: "status",
      header: "Status",
      cell: ({ row }) => (
        <Badge variant={getStatusBadgeVariant(row.original.status) as any}>{row.original.status}</Badge>
      ),
    },
    {
      id: "actions",
      cell: ({ row }) => (
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="ghost" size="icon">
              <MoreHorizontal className="h-4 w-4" />
              <span className="sr-only">Open menu</span>
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            <DropdownMenuItem onClick={() => handleEdit(row.original)}>
              <Edit className="mr-2 h-4 w-4" /> Edit
            </DropdownMenuItem>
            <DropdownMenuItem onClick={() => handleDelete(row.original)}>
              <Trash className="mr-2 h-4 w-4" /> Delete
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      ),
    },
  ]

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold tracking-tight">Payments</h1>
        <Button onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Payment
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Payment Management</CardTitle>
          <CardDescription>View and manage payment transactions</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading payments...</div>
          ) : (
            <DataTable
              columns={columns}
              data={payments}
              searchColumn="method"
              searchPlaceholder="Search by payment method..."
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>{currentPayment?.paymentId === 0 ? "Add New Payment" : "Edit Payment"}</DialogTitle>
              <DialogDescription>
                {currentPayment?.paymentId === 0 ? "Add a new payment transaction" : "Edit the payment information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="amount">Amount</Label>
                <Input
                  id="amount"
                  name="amount"
                  type="number"
                  min="0"
                  step="0.01"
                  value={currentPayment?.amount}
                  onChange={handleInputChange}
                  required
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="method">Payment Method</Label>
                <Select value={currentPayment?.method} onValueChange={(value) => handleSelectChange("method", value)}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a payment method" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="Credit Card">Credit Card</SelectItem>
                    <SelectItem value="Debit Card">Debit Card</SelectItem>
                    <SelectItem value="PayPal">PayPal</SelectItem>
                    <SelectItem value="Bank Transfer">Bank Transfer</SelectItem>
                    <SelectItem value="Cash">Cash</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="status">Status</Label>
                <Select value={currentPayment?.status} onValueChange={(value) => handleSelectChange("status", value)}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a status" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="Pending">Pending</SelectItem>
                    <SelectItem value="Completed">Completed</SelectItem>
                    <SelectItem value="Failed">Failed</SelectItem>
                    <SelectItem value="Refunded">Refunded</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>
            <DialogFooter>
              <Button type="submit">Save</Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>

      <Dialog open={isDeleteDialogOpen} onOpenChange={setIsDeleteDialogOpen}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Confirm Deletion</DialogTitle>
            <DialogDescription>
              Are you sure you want to delete this payment? This action cannot be undone.
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button variant="outline" onClick={() => setIsDeleteDialogOpen(false)}>
              Cancel
            </Button>
            <Button variant="destructive" onClick={confirmDelete}>
              Delete
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
