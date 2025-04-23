"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { useParams, useRouter } from "next/navigation"
import { ArrowLeft, Save } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Switch } from "@/components/ui/switch"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { CustomerAPI, AddressAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import { DataTable } from "@/components/ui/data-table"
import type { ColumnDef } from "@tanstack/react-table"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"

interface Customer {
  id: number
  name: string
  email: string
  age: number
  isActive: boolean
}

interface Address {
  addressId: number
  customerId: number
  line1: string
  line2: string
  city: string
  state: string
  zipCode: string
}

export default function CustomerDetailPage() {
  const params = useParams()
  const router = useRouter()
  const { toast } = useToast()
  const [customer, setCustomer] = useState<Customer | null>(null)
  const [addresses, setAddresses] = useState<Address[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isSaving, setIsSaving] = useState(false)
  const [isAddressDialogOpen, setIsAddressDialogOpen] = useState(false)
  const [currentAddress, setCurrentAddress] = useState<Address | null>(null)

  useEffect(() => {
    fetchCustomer()
  }, [params.id])

  const fetchCustomer = async () => {
    setIsLoading(true)
    try {
      const data = await CustomerAPI.getById(Number(params.id))
      setCustomer(data)
      fetchAddresses(Number(params.id))
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch customer details",
        variant: "destructive",
      })
      router.push("/customers")
    } finally {
      setIsLoading(false)
    }
  }

  const fetchAddresses = async (customerId: number) => {
    try {
      const data = await AddressAPI.getByCustomer(customerId)
      setAddresses(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch addresses",
        variant: "destructive",
      })
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value, type } = e.target

    if (!customer) return

    setCustomer({
      ...customer,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSwitchChange = (checked: boolean) => {
    if (!customer) return

    setCustomer({
      ...customer,
      isActive: checked,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!customer) return

    setIsSaving(true)
    try {
      await CustomerAPI.update(customer.id, customer)
      toast({
        title: "Success",
        description: "Customer updated successfully",
      })
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to update customer",
        variant: "destructive",
      })
    } finally {
      setIsSaving(false)
    }
  }

  const handleAddAddress = () => {
    setCurrentAddress({
      addressId: 0,
      customerId: Number(params.id),
      line1: "",
      line2: "",
      city: "",
      state: "",
      zipCode: "",
    })
    setIsAddressDialogOpen(true)
  }

  const handleEditAddress = (address: Address) => {
    setCurrentAddress(address)
    setIsAddressDialogOpen(true)
  }

  const handleDeleteAddress = async (addressId: number) => {
    try {
      await AddressAPI.delete(addressId)
      toast({
        title: "Success",
        description: "Address deleted successfully",
      })
      fetchAddresses(Number(params.id))
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete address",
        variant: "destructive",
      })
    }
  }

  const handleAddressInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target

    if (!currentAddress) return

    setCurrentAddress({
      ...currentAddress,
      [name]: value,
    })
  }

  const handleAddressSubmit = async (e: React.FormEvent) => {
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
      setIsAddressDialogOpen(false)
      fetchAddresses(Number(params.id))
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${currentAddress.addressId === 0 ? "create" : "update"} address`,
        variant: "destructive",
      })
    }
  }

  const addressColumns: ColumnDef<Address>[] = [
    {
      accessorKey: "addressId",
      header: "ID",
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
      header: "Actions",
      cell: ({ row }) => (
        <div className="flex space-x-2">
          <Button variant="outline" size="sm" onClick={() => handleEditAddress(row.original)}>
            Edit
          </Button>
          <Button variant="destructive" size="sm" onClick={() => handleDeleteAddress(row.original.addressId)}>
            Delete
          </Button>
        </div>
      ),
    },
  ]

  if (isLoading) {
    return <div className="flex items-center justify-center h-full">Loading customer details...</div>
  }

  return (
    <div className="space-y-4">
      <div className="flex items-center gap-2">
        <Button variant="outline" size="icon" onClick={() => router.push("/customers")}>
          <ArrowLeft className="h-4 w-4" />
          <span className="sr-only">Back</span>
        </Button>
        <h1 className="text-2xl font-bold tracking-tight">Customer Details</h1>
      </div>

      <Tabs defaultValue="details">
        <TabsList>
          <TabsTrigger value="details">Customer Details</TabsTrigger>
          <TabsTrigger value="addresses">Addresses</TabsTrigger>
        </TabsList>

        <TabsContent value="details">
          <Card>
            <form onSubmit={handleSubmit}>
              <CardHeader>
                <CardTitle>Customer Information</CardTitle>
                <CardDescription>Update customer account information</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="id">ID</Label>
                  <Input id="id" name="id" value={customer?.id} disabled />
                </div>

                <div className="space-y-2">
                  <Label htmlFor="name">Name</Label>
                  <Input id="name" name="name" value={customer?.name} onChange={handleInputChange} required />
                </div>

                <div className="space-y-2">
                  <Label htmlFor="email">Email</Label>
                  <Input
                    id="email"
                    name="email"
                    type="email"
                    value={customer?.email}
                    onChange={handleInputChange}
                    required
                  />
                </div>

                <div className="space-y-2">
                  <Label htmlFor="age">Age</Label>
                  <Input
                    id="age"
                    name="age"
                    type="number"
                    min="0"
                    value={customer?.age}
                    onChange={handleInputChange}
                    required
                  />
                </div>

                <div className="flex items-center space-x-2">
                  <Switch id="isActive" checked={customer?.isActive} onCheckedChange={handleSwitchChange} />
                  <Label htmlFor="isActive">Active Account</Label>
                </div>
              </CardContent>
              <CardFooter className="flex justify-end">
                <Button type="submit" disabled={isSaving}>
                  {isSaving ? (
                    "Saving..."
                  ) : (
                    <>
                      <Save className="mr-2 h-4 w-4" />
                      Save Changes
                    </>
                  )}
                </Button>
              </CardFooter>
            </form>
          </Card>
        </TabsContent>

        <TabsContent value="addresses">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between">
              <div>
                <CardTitle>Customer Addresses</CardTitle>
                <CardDescription>Manage addresses for this customer</CardDescription>
              </div>
              <Button size="sm" onClick={handleAddAddress}>
                Add Address
              </Button>
            </CardHeader>
            <CardContent>
              <DataTable
                columns={addressColumns}
                data={addresses}
                searchColumn="line1"
                searchPlaceholder="Search addresses..."
              />
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>

      <Dialog open={isAddressDialogOpen} onOpenChange={setIsAddressDialogOpen}>
        <DialogContent>
          <form onSubmit={handleAddressSubmit}>
            <DialogHeader>
              <DialogTitle>{currentAddress?.addressId === 0 ? "Add New Address" : "Edit Address"}</DialogTitle>
              <DialogDescription>
                {currentAddress?.addressId === 0
                  ? "Add a new address for this customer"
                  : "Update the address information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="line1">Address Line 1</Label>
                <Input
                  id="line1"
                  name="line1"
                  value={currentAddress?.line1}
                  onChange={handleAddressInputChange}
                  required
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="line2">Address Line 2 (Optional)</Label>
                <Input id="line2" name="line2" value={currentAddress?.line2} onChange={handleAddressInputChange} />
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div className="space-y-2">
                  <Label htmlFor="city">City</Label>
                  <Input
                    id="city"
                    name="city"
                    value={currentAddress?.city}
                    onChange={handleAddressInputChange}
                    required
                  />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="state">State</Label>
                  <Input
                    id="state"
                    name="state"
                    value={currentAddress?.state}
                    onChange={handleAddressInputChange}
                    required
                  />
                </div>
              </div>
              <div className="space-y-2">
                <Label htmlFor="zipCode">Zip Code</Label>
                <Input
                  id="zipCode"
                  name="zipCode"
                  value={currentAddress?.zipCode}
                  onChange={handleAddressInputChange}
                  required
                />
              </div>
            </div>
            <DialogFooter>
              <Button type="submit">Save Address</Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  )
}
