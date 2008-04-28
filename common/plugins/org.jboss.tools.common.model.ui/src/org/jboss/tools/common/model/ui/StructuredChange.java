/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class StructuredChange implements IStructuredChange {

	private List elements = null;

	public StructuredChange() {
	}
	public StructuredChange(List elements) {
		this.elements = elements;
	} 
	public StructuredChange(Object element) {
		this.elements = new ArrayList();
		elements.add(element);
	} 
	public StructuredChange(Object[] elements) {
		if (elements!=null) {
			this.elements = Arrays.asList(elements);
		}
	}
	
	// IStructuredChange

	public Object getFirstElement() {
		return ((this.elements!=null)&&(this.elements.size()>0))?elements.get(0):null;
	}

	public Iterator iterator() {
		return ((this.elements!=null)&&(this.elements.size()>0))?elements.iterator():null;
	}

	public int size() {
		return ((this.elements!=null)&&(this.elements.size()>0))?elements.size():0;
	}

	public Object[] toArray() {
		return ((this.elements!=null)&&(this.elements.size()>0))?elements.toArray():null;
	}

	public List toList() {
		// copy elements into new list
		ArrayList list = ((this.elements!=null)&&(this.elements.size()>0))?new ArrayList(elements):null; 
		return list;
	}

	// IChange
	
	public boolean isEmpty() {
		return ((this.elements!=null)&&(this.elements.size()>0))?false:true;
	}

}
