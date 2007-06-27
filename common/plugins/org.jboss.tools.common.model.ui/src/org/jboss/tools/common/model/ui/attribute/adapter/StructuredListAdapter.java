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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.StructuredChange;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class StructuredListAdapter extends DefaultValueAdapter 
	implements IStructuredChangeListener, IListContentProvider {

	public interface INewValueProvider {
		public Object getValue();
	}
	 
	protected static final String DEFAULT_DELIMITER = ";";
	
	ILabelProvider labelProvider = new LabelProvider();
	INewValueProvider newValueProvider = null;
	String delimiter = DEFAULT_DELIMITER;
	boolean trim = false;
	
	public void setAttribute(XAttribute attribute) {
		super.setAttribute(attribute);
		setDelimiter(attribute);
	}

	public void setAttributeData(XAttributeData data) {
		super.setAttributeData(data);
		setDelimiter(data.getAttribute());
	}
	
	private void setDelimiter(XAttribute attribute) {
		String s = attribute.getProperty("delimiter");
		if(s != null) {
			if(s.equals("comma")) s = ",";
		}
		delimiter = (s == null || s.length() != 1) ? DEFAULT_DELIMITER : s;
		trim = "true".equals(attribute.getProperty("trim"));
		String newValueClassName = attribute.getProperty("newValueClassName");
		if(newValueClassName != null) {
			try {
				newValueProvider = (INewValueProvider)ModelFeatureFactory.getInstance().createFeatureInstance(newValueClassName);
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
	}

	public Object getAdapter(Class adapter) {
		Object result = null;
		
		if (adapter == INewValueProvider.class) 
			result = newValueProvider;
		else if (adapter == IValueChangeListener.class) 
			result = this;
		else if (adapter == IStructuredChangeListener.class) 
			result = this;
		else if (adapter == IListContentProvider.class) 
			result = this;
		else if (adapter == ILabelProvider.class)
			result = labelProvider;
		else
			result = super.getAdapter(adapter); 
			
		return result;
	}

//		IStructuredChangeListener
	public void structureChanged(StructuredChangedEvent event) {
		StringBuffer newValue = new StringBuffer();
		StructuredChange change = (StructuredChange)event.getChange();
		Iterator iterator = change.iterator();
		if (iterator != null) 
			while (iterator.hasNext()) {
				newValue.append(iterator.next());
				if (iterator.hasNext()) newValue.append(delimiter);
			}
		setValue(newValue.toString());
	}

//		IListContentProvider
	public Object[] getElements(Object inputElement) {
		List<String> tokens = new ArrayList<String>();
		StringTokenizer tokenizer = new StringTokenizer(getStringValue(true), delimiter);
		while (tokenizer.hasMoreTokens()) {
			String t = tokenizer.nextToken();
			if(trim) t = t.trim();
			tokens.add(t);
		}
		return tokens.toArray();
	}
	
	public void dispose() {
		if (labelProvider !=null) labelProvider.dispose();
		labelProvider = null;
		newValueProvider = null;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
	
	public void setNewValueProvider(INewValueProvider provider) {
		newValueProvider = provider;		
	}

}
