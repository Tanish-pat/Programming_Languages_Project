"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Plus, Edit, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { AddressAPI, CustomerAPI } from "@/lib/api"
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

interface Address {
  addressId: number
  customerId: number
  line1: string
  line2: string
  city: string
  state: string
  zipCode: string
  customerName?: string
}

interface Customer {
  id: number
  name: string
  email: string
  phone: string
}


export default function AddressesPage() {
  const { toast } = useToast()
  const [addresses, setAddresses] = useState<Address[]>([])
  const [customers, setCustomers] = useState<any[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentAddress, setCurrentAddress] = useState<Address | null>(null)

  useEffect(() => {
    fetchAddresses()
    fetchCustomers()
  }, [])

  const fetchAddresses = async () => {
    setIsLoading(true)
    try {
      const rawData = await AddressAPI.getAll()
  
      const mappedAddresses: Address[] = await Promise.all(
        rawData.map(async (item: any[]) => {
          const address: Address = {
            addressId: item[0],
            customerId: item[1],
            line1: item[2],
            line2: item[3],
            city: item[4],
            state: item[5],
            zipCode: item[6],
          }
  
          try {
            const customer = await CustomerAPI.getById(address.customerId)
            console.log("Customer:", customer)
            return {
              ...address,
              customerName: customer[0][1],
            }
          } catch {
            return {
              ...address,
              customerName: "Unknown Customer",
            }
          }
        })
      )
  
      setAddresses(mappedAddresses)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch addresses",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const fetchCustomers = async () => {
    try {
      const rawData = await CustomerAPI.getAll()
      const mappedCustomers = rawData.map((item: any[]) => ({
        customerId: item[0],
        name: item[1],
        email: item[2],
        phone: item[3],
      }))
      setCustomers(mappedCustomers)
      console.log("Customers:", mappedCustomers)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch customers",
        variant: "destructive",
      })
    }
  }

  const handleAddNew = () => {
    setCurrentAddress({
      addressId: 0,
      customerId: 0,
      line1: "",
      line2: "",
      city: "",
      state: "",
      zipCode: "",
    })
    setIsDialogOpen(true)
  }

  const handleEdit = (address: Address) => {
    setCurrentAddress(address)
    setIsDialogOpen(true)
  }

  const handleDelete = (address: Address) => {
    setCurrentAddress(address)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentAddress) return

    try {
      await AddressAPI.delete(currentAddress.addressId)
      toast({
        title: "Success",
        description: "Address deleted successfully",
      })
      fetchAddresses()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete address",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target

    if (!currentAddress) return

    setCurrentAddress({
      ...currentAddress,
      [name]: value,
    })
  }

  const handleSelectChange = (value: string) => {
    if (!currentAddress) return

    setCurrentAddress({
      ...currentAddress,
      customerId: Number(value),
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentAddress) return

    try {
      if (currentAddress.addressId === 0) {
        await AddressAPI.create(currentAddress)
        toast({
          title: "Success",
          description: "Address created successfully",
        })
      } else {
        await AddressAPI.update(currentAddress.addressId, currentAddress)
        toast({
          title: "Success",
          description: "Address updated successfully",
        })
      }
      setIsDialogOpen(false)
      fetchAddresses()
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${currentAddress.addressId === 0 ? "create" : "update"} address`,
        variant: "destructive",
      })
    }
  }

  const columns: ColumnDef<Address>[] = [
    {
      accessorKey: "addressId",
      header: "ID",
    },
    {
      accessorKey: "customerName",
      header: "Customer",
    },
    {
      accessorKey: "line1",
      header: "Address Line 1",
    },
    {
      accessorKey: "city",
      header: "City",
    },
    {
      accessorKey: "state",
      header: "State",
    },
    {
      accessorKey: "zipCode",
      header: "Zip Code",
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
        <h1 className="text-2xl font-bold tracking-tight">Addresses</h1>
        <Button onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Address
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Address Management</CardTitle>
          <CardDescription>View and manage customer addresses</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading addresses...</div>
          ) : (
            <DataTable
              columns={columns}
              data={addresses}
              searchColumn="line1"
              searchPlaceholder="Search by address..."
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>{currentAddress?.addressId === 0 ? "Add New Address" : "Edit Address"}</DialogTitle>
              <DialogDescription>
                {currentAddress?.addressId === 0 ? "Add a new customer address" : "Edit the address information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="customerId">Customer</Label>
                <Select value={currentAddress?.customerId.toString()} onValueChange={handleSelectChange}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a customer" />
                  </SelectTrigger>
                  <SelectContent>

                    {customers.map((customer) => (
                      <SelectItem key={customer.customerId} value={customer.customerId.toString()}>
                        {customer.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="line1">Address Line 1</Label>
                <Input id="line1" name="line1" value={currentAddress?.line1} onChange={handleInputChange} required />
              </div>
              <div className="space-y-2">
                <Label htmlFor="line2">Address Line 2 (Optional)</Label>
                <Input id="line2" name="line2" value={currentAddress?.line2} onChange={handleInputChange} />
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div className="space-y-2">
                  <Label htmlFor="city">City</Label>
                  <Input id="city" name="city" value={currentAddress?.city} onChange={handleInputChange} required />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="state">State</Label>
                  <Input id="state" name="state" value={currentAddress?.state} onChange={handleInputChange} required />
                </div>
              </div>
              <div className="space-y-2">
                <Label htmlFor="zipCode">Zip Code</Label>
                <Input
                  id="zipCode"
                  name="zipCode"
                  value={currentAddress?.zipCode}
                  onChange={handleInputChange}
                  required
                />
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
              Are you sure you want to delete this address? This action cannot be undone.
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
