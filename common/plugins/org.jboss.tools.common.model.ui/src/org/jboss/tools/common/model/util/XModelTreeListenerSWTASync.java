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
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XModelTreeListenerSWTASync implements XModelTreeListener {
	private XModelTreeListener listener;
	
	
	Queue<XModelTreeEvent> queue = new ConcurrentLinkedQueue<XModelTreeEvent>();
	
	private Set<XModelObject> nodes = new HashSet<XModelObject>();
	
	Runnable runnable = null;
	
	private synchronized void add(XModelTreeEvent event) {
		if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
			if(nodes.contains(event.getModelObject())) {
				return;
			}
			queue.add(event);
		}

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
	
	public void nodeChanged(final XModelTreeEvent event) {
		run(event);
	}

	public void structureChanged(final XModelTreeEvent event) {
		run(event);
	}
	
	public synchronized void dispose() {
		listener = null;
		queue.clear();
	}
	
	public synchronized XModelTreeListener getListener() {
		return listener;
	}
	
	class R implements Runnable {
		public void run() {
			XModelTreeListener listener = getListener();
			if (listener != null) {
				for (XModelTreeEvent event : queue) {
					if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
						listener.nodeChanged(event);
					} else {
						listener.structureChanged(event);
					}
				}
			}
		}
	}
}
