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

public class XModelTreeListenerSWTASync implements XModelTreeListener {
	private XModelTreeListener listener;
	
	Queue<XModelTreeEvent> queue = new ConcurrentLinkedQueue<XModelTreeEvent>();
	
	private Set<XModelObject> nodes = new HashSet<XModelObject>();
	
	Runnable runnable = null;
	
	private synchronized void add(XModelTreeEvent event) {
		if(event.kind() == XModelTreeEvent.NODE_CHANGED) {
			//If event has other kind than NODE_CHANGED, it should never be filtered out!
			if(nodes.contains(event.getModelObject())) {
				return;
			}
			//Add to filter
			nodes.add(event.getModelObject());
		}
		//Add event to queue.
		queue.add(event);
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
				try {
					
					while (!queue.isEmpty()) {
						XModelTreeEvent event = queue.poll();
						synchronized(XModelTreeListenerSWTASync.this) {
							nodes.remove(event.getModelObject());
						}
						if (event.kind() == XModelTreeEvent.NODE_CHANGED) {
							listener.nodeChanged(event);
						} else {
							listener.structureChanged(event);
						}
					}
				} finally {
					runnable = null;
				}
			}
		}
	}
}
