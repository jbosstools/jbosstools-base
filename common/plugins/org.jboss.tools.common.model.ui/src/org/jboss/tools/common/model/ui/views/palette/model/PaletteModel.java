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
package org.jboss.tools.common.model.ui.views.palette.model;

import java.util.ArrayList;
import java.net.URL;
import java.net.MalformedURLException;

//import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.jface.resource.ImageDescriptor;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.views.palette.editor.*;
import org.jboss.tools.common.model.ui.util.ModelUtilities;

public class PaletteModel {
	static String SUB_GROUP_ELEMENT_TYPE = "sub-group";
	static String GROUP_ELEMENT_TYPE = "group";
	
	private static PaletteModel instance = null;
	private static Object monitor = new Object();

	private PaletteEditor editor = new PaletteEditor();

	private static final URL BASE_URL = ModelUIPlugin.getDefault().getBundle().getEntry("/");
	private IPaletteNode root = null;

	private PaletteModel() {
	}

	public static final PaletteModel getInstance() {
		if (instance != null) {
			return instance;
		} else {
			synchronized (monitor) {
				if (instance == null) {
					PaletteModel inst = new PaletteModel();
					inst.createModel();
					instance = inst;
				}
			}
			return instance;
		}
	}
	
	public IPaletteNode getRoot() {
		return root;
	}
	
	public void reload() {
		((PaletteGroup)root).clear();
		createModel();
	}
	
	private void createModel() {
		XModelObject xpalette = getXModelRoot();
		root = new PaletteGroup(xpalette);

		if (xpalette != null) {
			XModelObject[] xtabs = findXObjects(xpalette, GROUP_ELEMENT_TYPE);
			if (xtabs != null) { 
				for (int i = 0; i < xtabs.length; i++) {
					createTab(xtabs[i]);
				}
			}
		}
	}
	
	private void createTab(XModelObject xtab) {
		if (!"yes".equals(xtab.getAttributeValue("hidden"))) {
			IPaletteNode tab = addTab(xtab, xtab.getAttributeValue("name"));
	
			for (int j = 0; j < xtab.getChildren().length; j++) {
				XModelObject xelem = xtab.getChildAt(j);
				if (xelem.getAttributeValue("element type").equals("macro")) {
					createElem(tab, xelem, true);
				}
			}
		}
	}

	private void createElem(IPaletteNode parent, XModelObject xelem, boolean asGroup) {
		Thread.dumpStack();
	}
	
	private IPaletteNode addTab(XModelObject xobject, String title) {
		IPaletteNode tab = new PaletteGroup(xobject, title);
		root.addChild(tab);
		return tab;
	}

	public ImageDescriptor createImageDescriptor(String fileName) {
		String imagePath = "images/xstudio/palette/";
		try {
			URL url = new URL(BASE_URL, imagePath + fileName);
			return ImageDescriptor.createFromURL(url);
		} catch (MalformedURLException e) {
		}
		return ImageDescriptor.getMissingImageDescriptor();
	}

	private XModelObject[] findXObjects(XModelObject root, String elementType){
		ArrayList<XModelObject> v = new ArrayList<XModelObject>();
		for (int i = 0; i < root.getChildren().length; i++) {
			if (root.getChildAt(i).getAttributeValue("element type").equals(elementType)) {
				v.add(root.getChildAt(i));
			}
		}
		return (v.size() == 0) ? null : v.toArray(new XModelObject[0]);
	}

	private XModelObject getXModelRoot() {
		return getXModel().getRoot("Palette");
	}

	public XModel getXModel() {
		return ModelUtilities.getPreferenceModel();
	}
	
	public void openEditor(Shell shell) {
		editor.setObject(shell);
		editor.execute();
	}
	
	public void runShowHideDialog() {
		XModelObject root = getXModelRoot();
		XActionInvoker.invoke("HiddenTabs", root, new java.util.Properties());
	}

	public void addModelTreeListener(XModelTreeListener listener) {
		getXModel().addModelTreeListener(listener);
	}

	public void removeModelTreeListener(XModelTreeListener listener) {
		getXModel().removeModelTreeListener(listener);
	}
}
