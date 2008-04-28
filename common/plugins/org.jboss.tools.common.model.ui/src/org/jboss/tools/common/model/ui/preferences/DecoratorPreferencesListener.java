/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.preferences;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.navigator.decorator.DecoratorManager;

/**
 * @author Viacheslav Kabanovich
 */
public class DecoratorPreferencesListener implements IPropertyChangeListener {
	StructuredViewer viewer;
	
	public DecoratorPreferencesListener() {}
	
	public void setViewer(StructuredViewer viewer) {
		this.viewer = viewer;
	}
	
	public void init() {
		ModelUIPlugin.getDefault().getPreferenceStore().addPropertyChangeListener(this);
	}
	
	public void dispose() {
		ModelUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener(this);
	}

	public void propertyChange(PropertyChangeEvent event) {
		String s = event.getProperty();
		if(s.startsWith(DecoratorManager.EXTENSION_POINT_ID + ".")) {
			if(viewer != null && viewer.getControl() != null && !viewer.getControl().isDisposed()) {
				viewer.refresh(true);
			}
		}		
	}
	
}
