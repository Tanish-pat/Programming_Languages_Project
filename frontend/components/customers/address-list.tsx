"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Plus, Edit, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from "@/components/ui/table"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { AddressAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"

interface Address {
  addressId: number
  customerId: number
  line1: string
  line2: string
  city: string
  state: string
  zipCode: string
}

interface AddressListProps {
  customerId: number
}

export default function AddressList({ customerId }: AddressListProps) {
  const { toast } = useToast()
  const [addresses, setAddresses] = useState<Address[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [currentAddress, setCurrentAddress] = useState<Address | null>(null)
  const [isSaving, setIsSaving] = useState(false)

  useEffect(() => {
    fetchAddresses()
  }, [customerId])

  const fetchAddresses = async () => {
    setIsLoading(true)
    try {
      const data = await AddressAPI.getByCustomer(customerId)
      setAddresses(data)
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

  const handleAddNew = () => {
    setCurrentAddress({
      addressId: 0,
      customerId,
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

  const handleDelete = async (addressId: number) => {
    try {
      await AddressAPI.delete(addressId)
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

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentAddress) return

    setIsSaving(true)
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
    } finally {
      setIsSaving(false)
    }
  }

  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between">
        <div>
          <CardTitle>Customer Addresses</CardTitle>
          <CardDescription>Manage addresses for this customer</CardDescription>
        </div>
        <Button size="sm" onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Address
        </Button>
      </CardHeader>
      <CardContent>
        <div className="rounded-md border">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>ID</TableHead>
                <TableHead>Address</TableHead>
                <TableHead>City</TableHead>
                <TableHead>State</TableHead>
                <TableHead>Zip Code</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={6} className="text-center py-8">
                    Loading addresses...
                  </TableCell>
                </TableRow>
              ) : addresses.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={6} className="text-center py-8">
                    No addresses found
                  </TableCell>
                </TableRow>
              ) : (
                addresses.map((address) => (
                  <TableRow key={address.addressId}>
                    <TableCell>{address.addressId}</TableCell>
                    <TableCell>
                      {address.line1}
                      {address.line2 && <div className="text-sm text-muted-foreground">{address.line2}</div>}
                    </TableCell>
                    <TableCell>{address.city}</TableCell>
                    <TableCell>{address.state}</TableCell>
                    <TableCell>{address.zipCode}</TableCell>
                    <TableCell className="text-right">
                      <DropdownMenu>
                        <DropdownMenuTrigger asChild>
                          <Button variant="ghost" size="icon">
                            <MoreHorizontal className="h-4 w-4" />
                            <span className="sr-only">Open menu</span>
                          </Button>
                        </DropdownMenuTrigger>
                        <DropdownMenuContent align="end">
                          <DropdownMenuLabel>Actions</DropdownMenuLabel>
                          <DropdownMenuSeparator />
                          <DropdownMenuItem onClick={() => handleEdit(address)}>
                            <Edit className="mr-2 h-4 w-4" /> Edit
                          </DropdownMenuItem>
                          <DropdownMenuItem onClick={() => handleDelete(address.addressId)}>
                            <Trash className="mr-2 h-4 w-4" /> Delete
                          </DropdownMenuItem>
                        </DropdownMenuContent>
                      </DropdownMenu>
                    </TableCell>
                  </TableRow>
                ))
              )}
            </TableBody>
          </Table>
        </div>

        <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
          <DialogContent>
            <form onSubmit={handleSubmit}>
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
                    <Input
                      id="state"
                      name="state"
                      value={currentAddress?.state}
                      onChange={handleInputChange}
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
                    onChange={handleInputChange}
                    required
                  />
                </div>
              </div>
              <DialogFooter>
                <Button type="submit" disabled={isSaving}>
                  {isSaving ? "Saving..." : "Save Address"}
                </Button>
              </DialogFooter>
            </form>
          </DialogContent>
        </Dialog>
      </CardContent>
    </Card>
  )
}
