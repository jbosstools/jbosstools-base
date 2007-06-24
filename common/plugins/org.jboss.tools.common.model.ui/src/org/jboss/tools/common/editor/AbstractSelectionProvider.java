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
package org.jboss.tools.common.editor;

import java.util.*;
import org.eclipse.jface.viewers.*;

import org.jboss.tools.common.model.XModelObject;

public abstract class AbstractSelectionProvider implements ISelectionProvider {
	private ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>();

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		if(!listeners.contains(listener)) listeners.add(listener);
	}

	public ISelection getSelection() {
		XModelObject o = getSelectedModelObject(); 				
		return (o == null) ? new StructuredSelection() : new StructuredSelection(o);
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		listeners.remove(listener);	
	}

	public void setSelection(ISelection selection) {
		if(!(selection instanceof StructuredSelection)) return;
		StructuredSelection ss = (StructuredSelection)selection;		
		if(!ss.isEmpty() && (ss.getFirstElement() instanceof XModelObject)) 
			setSelectedModelObject((XModelObject)ss.getFirstElement());
	}

	public void fireSelectionChanged() {
		if(listeners.isEmpty()) return;
		ISelection selection = getSelection();
		if(selection == null) return; 
		SelectionChangedEvent newEvent = new SelectionChangedEvent(this, selection);
		Iterator iterator = listeners.iterator();
		while (iterator.hasNext())
			((ISelectionChangedListener)iterator.next()).selectionChanged(newEvent);
	}
	
	protected abstract XModelObject getSelectedModelObject();
	protected abstract void setSelectedModelObject(XModelObject object);
	
}
