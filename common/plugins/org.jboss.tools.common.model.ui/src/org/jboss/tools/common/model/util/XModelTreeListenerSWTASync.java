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
package org.jboss.tools.common.model.util;

import java.util.*;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XModelTreeListenerSWTASync implements XModelTreeListener {
	private XModelTreeListener listener;
	
	private Item head = null;
	private Item tail = null;
	private Set<XModelObject> nodes = new HashSet<XModelObject>();
	Runnable runnable = null;
	
	private synchronized void add(XModelTreeEvent event) {
		if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
			if(nodes.contains(event.getModelObject())) {
				return;
			}
			nodes.add(event.getModelObject());
		}
		Item item = new Item();
		item.event = event;		
		if(head == null) {
			head = item;
			tail = item;
		} else {
			tail.next = item;
			tail = item;
		}
	}
	
	private synchronized XModelTreeEvent get() {
		if(head == null) {
			runnable = null;
			return null;
		}
		XModelTreeEvent event = head.event;
		head = head.next;
		if(head == null) tail = null;
		if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
			nodes.remove(event.getModelObject());
		}
		return event;
	}
	
	private void run(XModelTreeEvent event) {
		add(event);
		synchronized(this) {
			if(runnable != null) return;
			runnable = new R();
		}
		Display.getDefault().asyncExec(runnable);
	}
	
	public XModelTreeListenerSWTASync(XModelTreeListener listener){
		this.listener = listener;
	}
	
	static int count = 0;
	
	public void nodeChanged(final XModelTreeEvent event) {
		run(event);
	}

	public void structureChanged(final XModelTreeEvent event) {
		run(event);
	}
	
	public void dispose() {
		listener = null;
	}
	
	class Item {
		XModelTreeEvent event;
		Item next;
	}
	
	
	class R implements Runnable {
		public void run() {
			++count;
			XModelTreeListener l = listener;
			if (listener == null) {
				ModelUIPlugin.getPluginLog().logInfo("ModelListener is disposed, but cannot removed from model!!!!");
				head = null;
				tail = null;
				return;				
			}
			XModelTreeEvent event = null;
			while((event = get()) != null) {
				if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
					l.nodeChanged(event);
				} else {
					l.structureChanged(event);
				}
			}
		}
	}
}
