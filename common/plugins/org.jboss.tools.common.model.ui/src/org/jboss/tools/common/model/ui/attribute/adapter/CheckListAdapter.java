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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.StringTokenizer;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class CheckListAdapter extends DefaultValueAdapter {
	public Image IMAGE_ENABLED = EclipseResourceUtil.getImage("images/common/check.gif"); //$NON-NLS-1$
	public Image IMAGE_DISABLED = EclipseResourceUtil.getImage("images/common/uncheck.gif"); //$NON-NLS-1$
	
	protected ILabelProvider labelProvider;
	protected ITreeContentProvider treeContentProvider;

	private STCPSpecialHelperSupportHandler provider;
	private String[] tags = new String[]{"tag1", "tag2"};
	
	public CheckListAdapter() {
		labelProvider = new STCPTreeLabelProvider();
	}
	
	public void dispose() {
		super.dispose();
		if (treeContentProvider!=null) treeContentProvider.dispose();
		treeContentProvider = null;
		if (labelProvider!=null) labelProvider.dispose();
		labelProvider = null;
		if (provider!=null) provider.dispose();
		provider = null;
	}

	public void setTags(String[] tags) {
		if (provider==null || !(provider instanceof STCPSpecialHelperSupportHandler)) provider = new STCPSpecialHelperSupportHandler();
		if(!differ(this.tags, tags)) return;
		this.tags = tags;
		if(provider.viewer != null) provider.viewer.refresh();		
	}
	
	public void checkAll() {
		StringBuffer sb = new StringBuffer();
		char separator = getSeparator();
		for (int i = 0; i < tags.length; i++) {
			if(i > 0) sb.append(separator);
			sb.append(tags[i]);
		}
		setValue(sb.toString());
	}
	
	public void setValue(Object value) {
		super.setValue(value);
		if (provider!=null && provider.viewer != null && !provider.viewer.getControl().isDisposed()) provider.viewer.refresh();		
	}
	
	public String[] getTags() {
		return tags;
	}
	
	private boolean differ(String[] s1, String[] s2) {
		if(s1.length != s2.length) return true;
		for (int i = 0; i < s1.length; i++) if(!s1[i].equals(s2[i])) return true;
		return false;
	}

	protected ITreeContentProvider createTreeContentProvider(XAttribute attribute) {
		return new DefaultXAttributeTreeContentProvider(attribute, model, modelObject);
	}

	public Object getAdapter(Class adapter) {
		if (adapter == ILabelProvider.class) {
			return labelProvider;
		}
		if (adapter == ITreeContentProvider.class) {
			if (provider!=null) return provider;
			if (treeContentProvider==null) {
				treeContentProvider = createTreeContentProvider(attribute);
			}
			return treeContentProvider;
		}
		return super.getAdapter(adapter);
	}
	
	class STCPTreeLabelProvider extends LabelProvider {
		/**
		 * Tree style SWT.CHECK is used instead.
		public Image getImage(Object element) {
			String s = getStringValue(true);
			StringTokenizer st = new StringTokenizer(s, ";,");
			while(st.hasMoreTokens()) {
				if(st.nextToken().equals(element)) return IMAGE_ENABLED;
			}
			return IMAGE_DISABLED;
		}
		*/
	}
	
	class STCPSpecialHelperSupportHandler implements ITreeContentProvider {
		Viewer viewer;

		public Object[] getElements(Object inputElement) {
			return tags;
		}

		public void dispose() {
			viewer = null;
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			this.viewer = viewer;
			if(viewer != null) viewer.refresh();
		}
		
		void ignore() {}

		public Object[] getChildren(Object parentElement) {
			return new Object[0];
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return false;
		}
	}

	public char getSeparator() {
		XAttribute a = getAttribute();
		if(a == null && getAttributeData() != null) {
			a = getAttributeData().getAttribute();
		}
		if(a != null) {
			String s = a.getProperty("separator");
			if("comma".equals(s)) return ',';
		}
		return ';';
	}
}
