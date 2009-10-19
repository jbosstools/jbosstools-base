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
package org.jboss.tools.common.model.ui.action;

import java.util.*;

import org.jboss.tools.common.model.ui.dnd.DnDUtil;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Menu;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.XModelObject;

public abstract class XMenuInvoker implements MouseListener, KeyListener {
	protected Viewer viewer;
	protected boolean onKeyRelease = false;

	ModelContributionManager standardInvoker;

	public XMenuInvoker() {}

	public void setStandardInvoker(ModelContributionManager standardInvoker) {
		this.standardInvoker = standardInvoker;
	}

	public void setViewer(Viewer viewer) {
		this.viewer = viewer;
	}
	
	public void setOnKeyRelease(boolean b) {
		onKeyRelease = b;
	}

	public void mouseDoubleClick(MouseEvent e) {
	}

	public void mouseDown(MouseEvent e) {
		if(standardInvoker != null) {
			XModelObject eo = getModelObjectAt(new Point(e.x, e.y));
			XModelObject o = getSelectedModelObject();
			if(o == null && eo == null) return;
			XModelObject[] os = getSelectedModelObjects();
			if(eo != null && (o == null || os == null || os.length < 2)) {
				o = eo;
				os = null;
			} else if(isIncluded(eo, os)) {
				o = eo;
			}
			if(os != null && os.length > 0) {
				standardInvoker.setSelection(new StructuredSelection(os));
			} else if(o != null) {
				standardInvoker.setSelection(new StructuredSelection(o));
			}
		}
	}

	public void mouseUp(MouseEvent e) {
		handleMouseUp(e);
	}
	
	protected void handleMouseUp(MouseEvent e) {
		if(e.button == 3 && standardInvoker == null) { 
			XModelObject eo = getModelObjectAt(new Point(e.x, e.y));
			XModelObject o = getSelectedModelObject();
			if(o == null && eo == null) return;
			XModelObject[] os = getSelectedModelObjects();
			if(eo != null && (o == null || os == null || os.length < 2)) {
				o = eo;
				os = null;
			} else if(isIncluded(eo, os)) {
				o = eo;
			}
			XModelObjectActionList l = new XModelObjectActionList(getActionList(o), o, os, new Object[]{o, getRunningProperties()});				
			Menu menu = l.createMenu(viewer.getControl());
			menu.setVisible(true);
		}
		if(e.button == 1) { 
			XModelObject o = getSelectedModelObject();
			if(o == null) return;
			if("true".equals(o.get("overlapped"))) { //$NON-NLS-1$ //$NON-NLS-2$
				XActionInvoker.invoke("Open", o, new Properties()); //$NON-NLS-1$
			}
		}
	}
	
	private boolean isIncluded(XModelObject object, XModelObject[] objects) {
		if(object == null || objects == null) return false;
		for (int i = 0; i < objects.length; i++) if(objects[i] == object) return true;
		return false;
	}

	protected XActionList getActionList(XModelObject o) {
		return o.getModelEntity().getActionList();
	}

	public abstract XModelObject getSelectedModelObject();
	
	public XModelObject[] getSelectedModelObjects() {
		return null;
	}
	
	public XModelObject getModelObjectAt(Point p) {
		return null;
	}
	
	public void keyPressed(KeyEvent e) {
		if(!onKeyRelease) processKey(e);
	}

	public void keyReleased(KeyEvent e) {
		if(onKeyRelease) processKey(e);
	}
	
	protected void processKey(KeyEvent e) {
		if(((e.stateMask & SWT.CTRL) != 0) && e.keyCode == (int)'c') {
			invoke("CopyActions.Copy"); //$NON-NLS-1$
		} else if(((e.stateMask & SWT.CTRL) != 0) && e.keyCode == (int)'x') {
			invoke("CopyActions.Cut"); //$NON-NLS-1$
		} else if(((e.stateMask & SWT.CTRL) != 0) && e.keyCode == (int)'v') {
			invoke("CopyActions.Paste"); //$NON-NLS-1$
		} else if((e.stateMask == 0) && e.character == SWT.DEL) {
			invoke("DeleteActions.Delete"); //$NON-NLS-1$
		}
	}

	private void invoke(String actionPath) {
		XModelObject object = getSelectedModelObject();
		if(object == null) return;
		XModelObject[] objects = getSelectedModelObjects();
		if(DnDUtil.getEnabledAction(object, objects, actionPath) == null) return;
			if(objects == null) {
				XActionInvoker.invoke(actionPath, object, getRunningProperties());
			} else {
				XActionInvoker.invoke(actionPath, object, objects, getRunningProperties());
			}
		
	}
	
	private Properties getRunningProperties() {
		Properties p = new Properties();
		fillRunningProperties(p);
		return p;
	}
	
	protected void fillRunningProperties(Properties p) {}

}
