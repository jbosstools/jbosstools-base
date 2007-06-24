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

import java.util.ArrayList;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;

public class SelectionNotifier implements ISelectionProvider, ISelectionChangedListener {
	private ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>(3);
	private ArrayList<ISelectionChangedListener> fires = new ArrayList<ISelectionChangedListener>(3);
	private SelectionChangedEvent event;
	private ISelection selection;
		
	// ISelectionChangedListener 		
	public void selectionChanged(SelectionChangedEvent event) {
		if ((event==null)&&(this.event==null)) return;
		if ((event!=null)&&(this.event!=null)) {
			if ((event == this.event)||(event.equals(this.event))) return;
		}
		this.event = event;
		if (event!=null) {
			selection = event.getSelection();
		} else {
			selection = null;
		}
		this.fireSelectionChanged();
	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		listeners.add(listener); 
	}

	public ISelection getSelection() {
		return selection;
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		listeners.remove(listener); 
	}

	public void setSelection(ISelection selection) {
		this.selection = selection;
		event = new SelectionChangedEvent(this, selection);
		fireSelectionChanged();
	}
		
	protected void fireSelectionChanged() {
		fires.addAll(listeners); // copy listeners
		if(fires.isEmpty()) return;
		for (int i=0;i<fires.size();++i) ((ISelectionChangedListener)fires.get(i)).selectionChanged(event);
		fires.clear();
	}
}
