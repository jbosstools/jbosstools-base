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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import java.util.ArrayList;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultComboBoxValueAdapter;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstallType;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;

public class TomcatJVMListAdapter extends DefaultComboBoxValueAdapter implements IListContentProvider {
	
	protected String[] tags = new String[0];	

	public TomcatJVMListAdapter(){
	}

	protected IListContentProvider createListContentProvider(XAttribute attribute) {
		return this;	
	}

	public void dispose() {
	}

	public Object[] getElements(Object inputElement) {
		if(tags.length==0) 
			loadTags();
		return tags;		
	}

	public void inputChanged(
		Viewer viewer,
		Object oldInput,
		Object newInput) {
	}
	 
	protected void loadTags() {
		ArrayList<String> list = new ArrayList<String>();
		IVMInstallType[] jvmType = JavaRuntime.getVMInstallTypes();
		for (int i = 0; i < jvmType.length; i++) {
			IVMInstall[] jvmInstall = jvmType[i].getVMInstalls();
			for (int j = 0; j < jvmInstall.length; j++) {
				list.add(jvmInstall[j].getName());	
			}
		}
		tags = list.toArray(new String[0]);				
	}

	public void propertyChange(PropertyChangeEvent event) {
	}

	public Object getAdapter(Class adapter) {
		if(adapter==ILabelProvider.class) {
			return new TomcatAdapterList();
		} 
		return super.getAdapter(adapter);
	}
	
	static class TomcatAdapterList extends LabelProvider { 
		public String getText(Object text) {
			IVMInstallType[] jvmType = JavaRuntime.getVMInstallTypes();
			for (int i = 0; i < jvmType.length; i++) {
				IVMInstall[] jvmInstall = jvmType[i].getVMInstalls();
				for (int j = 0; j < jvmInstall.length; j++) {
					if(jvmInstall[j].getName().equals(text)) return jvmInstall[j].getName() + " - \"" + (jvmInstall[j].getInstallLocation().getPath().length()>50?jvmInstall[j].getInstallLocation().getPath().substring(50)+" ...":jvmInstall[j].getInstallLocation().getPath()) +"\"";	
				}
			}
			return "";
		}
	}
}
