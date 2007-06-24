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

import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

import org.eclipse.swt.widgets.Display;

/**
 * @author Daniel
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class XModelTreeListenerSWTSync implements XModelTreeListener{
	private XModelTreeListener listener;
	
	public XModelTreeListenerSWTSync(XModelTreeListener listener){
		this.listener = listener;
	}

	public void nodeChanged(final XModelTreeEvent event) {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					if(listener != null) {
						listener.nodeChanged(event);
					} else {
						ModelUIPlugin.log("ModelListener is disposed, but not removed from model!!!!");
					}
				}
			}
		);
	}

	public void structureChanged(final XModelTreeEvent event) {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					if(listener != null) {
						listener.structureChanged(event);
					} else {
						ModelUIPlugin.log("ModelListener is disposed, but not removed from model!!!!");
					}
				}
			}
		);
	}
	
	public void dispose() {
		listener = null;
	}
}
