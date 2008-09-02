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
package org.jboss.tools.common.resref.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.attribute.editor.DirectoryFieldEditorEx;

public abstract class FolderReferenceComposite {
	XAttributeSupport support = new XAttributeSupport();
	XModelObject object;
	IFile file;
	IPath path;
	ResourceReference[] rs;
	ResourceReference current;
	XEntityData data;
	
	public FolderReferenceComposite() {
		object = PreferenceModelUtilities.getPreferenceModel().createModelObject(getEntity(), new Properties());
	}
	
	public void setObject(Properties p) {
		file = (IFile)p.get("file");
		path = (IPath)p.get("path");
		rs = (file != null) ? getReferenceList().getAllResources(file) :
							(path != null) ? getReferenceList().getAllResources(path)
							               : new ResourceReference[0];
		if(rs.length == 0) {
			rs = new ResourceReference[1];
			rs[0] = new ResourceReference("", ResourceReference.FILE_SCOPE);
		}
		current = rs[0];
		object.setAttributeValue("location", current.getLocation());
		object.setAttributeValue("scope", current.getScopeName());

		data = XEntityDataImpl.create(new String[][]{
			{getEntity(), "yes"},
			{"location", "no"},
			{"scope", "no"}
		});

		data.getAttributeData()[0].setValue(current.getLocation());
		data.getAttributeData()[1].setValue(current.getScopeName());

		support.init(object, data);
//		support.addPropertyChangeListener(new PropertyChangeListener() {
//			public void propertyChange(PropertyChangeEvent evt) {
//			}
//		});
	}

	protected String getEntity() {
		return (file != null) ? "VPEFolderReference" : "VPEFolderReferenceExt";
	}

	protected abstract ResourceReferenceList getReferenceList();
	protected abstract String getTitle();
	
	public Control createControl(Composite parent) {
		Group g = new Group(parent, SWT.SHADOW_ETCHED_IN);
		GridLayout layout = new GridLayout(1, false);
		g.setLayout(layout);
		g.setText(getTitle());
		Control c = support.createControl(g);
		if(file != null) {
			DirectoryFieldEditorEx f = (DirectoryFieldEditorEx)support.getFieldEditorByName("location");
			f.setLastPath(file.getProject().getLocation().toString());
		}
		GridData data = new GridData(GridData.FILL_BOTH);
		c.setLayoutData(data);
		return g;
	}
	
	public void commit() {
		support.store();
		current.setLocation(data.getAttributeData()[0].getValue());
		current.setScope(getNewScope());
		List l = new ArrayList();
		for (int i = rs.length - 2; i >= 0; i--) {
			if(rs[i].getLocation().equals(current.getLocation())) continue;
			if(rs[i].getScope() == current.getScope()) continue;
			l.add(rs[i]);
		}
		l.add(current);
		rs = (ResourceReference[])l.toArray(new ResourceReference[0]);
		if(file != null) {
			getReferenceList().setAllResources(file, rs);
		} else {
			getReferenceList().setAllResources(path, rs);
		}
	}
	
	int getNewScope() {
		String s = data.getAttributeData()[1].getValue();
		for (int i = 0; i < ResourceReference.SCOPE_NAMES.length; i++) {
			if(ResourceReference.SCOPE_NAMES[i].equals(s)) return i;
		}
		return ResourceReference.FILE_SCOPE;
	}

}
