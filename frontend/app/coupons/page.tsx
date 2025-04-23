"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Plus, Edit, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { Badge } from "@/components/ui/badge"
import { CouponAPI } from "@/lib/api"
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
import { Switch } from "@/components/ui/switch"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"
import type { ColumnDef } from "@tanstack/react-table"

interface Coupon {
  couponCode: string
  discount: number
  isActive: boolean
}

export default function CouponsPage() {
  const { toast } = useToast()
  const [coupons, setCoupons] = useState<Coupon[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentCoupon, setCurrentCoupon] = useState<Coupon | null>(null)

  useEffect(() => {
    fetchCoupons()
  }, [])

  const fetchCoupons = async () => {
    setIsLoading(true)
    try {
      const rawData = await CouponAPI.getAll()
      const mappedCoupons: Coupon[] = rawData.map((item: any[]) => ({
        couponCode: item[0],
        discount: Number(item[1]),
        isActive: item[2] === 1,
      }))
      setCoupons(mappedCoupons)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch coupons",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const handleAddNew = () => {
    setCurrentCoupon({
      couponCode: "",
      discount: 0,
      isActive: true,
    })
    setIsDialogOpen(true)
  }

  const handleEdit = (coupon: Coupon) => {
    setCurrentCoupon(coupon)
    setIsDialogOpen(true)
  }

  const handleDelete = (coupon: Coupon) => {
    setCurrentCoupon(coupon)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentCoupon) return

    try {
      await CouponAPI.delete(currentCoupon.couponCode)
      toast({
        title: "Success",
        description: "Coupon deleted successfully",
      })
      fetchCoupons()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete coupon",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value, type } = e.target

    if (!currentCoupon) return

    setCurrentCoupon({
      ...currentCoupon,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSwitchChange = (checked: boolean) => {
    if (!currentCoupon) return

    setCurrentCoupon({
      ...currentCoupon,
      isActive: checked,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentCoupon) return

    try {
      // Check if coupon exists by code
      const isNewCoupon = !coupons.some((c) => c.couponCode === currentCoupon.couponCode)

      if (isNewCoupon) {
        await CouponAPI.create(currentCoupon)
        toast({
          title: "Success",
          description: "Coupon created successfully",
        })
      } else {
        await CouponAPI.update(currentCoupon.couponCode, currentCoupon)
        toast({
          title: "Success",
          description: "Coupon updated successfully",
        })
      }
      setIsDialogOpen(false)
      fetchCoupons()
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${!coupons.some((c) => c.couponCode === currentCoupon.couponCode) ? "create" : "update"} coupon`,
        variant: "destructive",
      })
    }
  }

  const columns: ColumnDef<Coupon>[] = [
    {
      accessorKey: "couponCode",
      header: "Coupon Code",
    },
    {
      accessorKey: "discount",
      header: "Discount",
      cell: ({ row }) => `${row.original.discount}%`,
    },
    {
      accessorKey: "isActive",
      header: "Status",
      cell: ({ row }) => (
        <Badge variant={row.original.isActive ? "success" : "secondary"}>
          {row.original.isActive ? "Active" : "Inactive"}
        </Badge>
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
        <h1 className="text-2xl font-bold tracking-tight">Coupons</h1>
        <Button onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Coupon
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Coupon Management</CardTitle>
          <CardDescription>View and manage discount coupons</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading coupons...</div>
          ) : (
            <DataTable
              columns={columns}
              data={coupons}
              searchColumn="couponCode"
              searchPlaceholder="Search by coupon code..."
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>
                {!coupons.some((c) => c.couponCode === currentCoupon?.couponCode) ? "Add New Coupon" : "Edit Coupon"}
              </DialogTitle>
              <DialogDescription>
                {!coupons.some((c) => c.couponCode === currentCoupon?.couponCode)
                  ? "Add a new discount coupon"
                  : "Edit the coupon information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="couponCode">Coupon Code</Label>
                <Input
                  id="couponCode"
                  name="couponCode"
                  value={currentCoupon?.couponCode}
                  onChange={handleInputChange}
                  required
                  disabled={coupons.some((c) => c.couponCode === currentCoupon?.couponCode)}
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="discount">Discount (%)</Label>
                <Input
                  id="discount"
                  name="discount"
                  type="number"
                  min="0"
                  max="100"
                  value={currentCoupon?.discount}
                  onChange={handleInputChange}
                  required
                />
              </div>
              <div className="flex items-center space-x-2">
                <Switch id="isActive" checked={currentCoupon?.isActive} onCheckedChange={handleSwitchChange} />
                <Label htmlFor="isActive">Active Coupon</Label>
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
              Are you sure you want to delete coupon "{currentCoupon?.couponCode}"? This action cannot be undone.
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
