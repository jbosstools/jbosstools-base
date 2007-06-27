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
package org.jboss.tools.common.model.ui.forms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.ui.IMemento;

import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author AU
 */
public class Memento implements IMemento {
	
	private static final String MEMENTO = "M";
	private static final String DIVIDER = ":";
	private static final String LINE = "|";
	
	private static final String ARRAY = "A";
	private static final String FLOAT = "F";
	private static final String INTEGER = "I";
	private static final String STRING = "S";
	
	private String id;
	private HashMap map = new HashMap();
	private static final String TEXT_DATA = "org.jboss.tools.common.model.ui.forms.Memento.textData.id";


	private Memento() {}
	
	public Memento(String id) {
		ModelUIPlugin.getPluginLog().logInfo("new Memento("+id+")");
		this.id = id;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#createChild(java.lang.String)
	 */
	public IMemento createChild(String type) {
		return createChild(type, type);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#createChild(java.lang.String, java.lang.String)
	 */
	public IMemento createChild(String type, String id) {
		Memento newMemento = new Memento(id);
		ArrayList list = (ArrayList)map.get(type);
		if (list==null) {
			list = new ArrayList();
			map.put(type, list);
		} 
		list.add(newMemento);
		return newMemento;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getChild(java.lang.String)
	 */
	public IMemento getChild(String type) {
		ArrayList list = (ArrayList)map.get(type);
		if (list!=null && list.size()>0) 
			return (IMemento)list.get(0); 
			return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getChildren(java.lang.String)
	 */
	public IMemento[] getChildren(String type) {
		ArrayList list = (ArrayList)map.get(type);
		if (list!=null)
			return (IMemento[])list.toArray(new IMemento[list.size()]);
			return new IMemento[0];
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getFloat(java.lang.String)
	 */
	public Float getFloat(String key) {
		return (Float)map.get(key);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getID()
	 */
	public String getID() {
		return id;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getInteger(java.lang.String)
	 */
	public Integer getInteger(String key) {
		return (Integer)map.get(key);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getString(java.lang.String)
	 */
	public String getString(String key) {
		return (String)map.get(key);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getTextData()
	 */
	public String getTextData() {
		return (String)map.get(TEXT_DATA);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putFloat(java.lang.String, float)
	 */
	public void putFloat(String key, float value) {
		map.put(key, new Float(value));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putInteger(java.lang.String, int)
	 */
	public void putInteger(String key, int value) {
		map.put(key, new Integer(value));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putMemento(org.eclipse.ui.IMemento)
	 */
	public void putMemento(IMemento memento) {
		if (memento!=null) map.put(memento.getID(), memento);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putString(java.lang.String, java.lang.String)
	 */
	public void putString(String key, String value) {
		map.put(key, value);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putTextData(java.lang.String)
	 */
	public void putTextData(String data) {
		map.put(TEXT_DATA, data);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return super.toString()+" id="+id;
	}
	
	public String stringify() {
		StringBuffer sb = new StringBuffer();
		// head
		sb.append(MEMENTO);
		sb.append(DIVIDER);
		sb.append(id);
		sb.append(LINE);
		// body
		Set keys = map.keySet();
		Iterator i = keys.iterator();
		String key;
		Object value; 
		while (i.hasNext()) {
			key = (String)i.next(); 
			value = map.get(key);
			if (value instanceof Integer) {
				sb.append(INTEGER);
				sb.append(DIVIDER);
				sb.append(key);
				sb.append(DIVIDER);
				sb.append(((Integer)value).intValue());
			} else if (value instanceof String) {
				sb.append(STRING);
				sb.append(DIVIDER);
				sb.append(key);
				sb.append(DIVIDER);
				sb.append((String)value);
			} else if (value instanceof ArrayList) {
				sb.append(ARRAY);
				sb.append(DIVIDER);
				sb.append(key);
				sb.append(DIVIDER);
				ArrayList list = (ArrayList)value;
				Iterator j = list.iterator();
				while (j.hasNext()) {
					Memento children = (Memento)j.next();
					sb.append(children.stringify());
				}
			} else if (value instanceof Float) {
				sb.append(FLOAT);
				sb.append(DIVIDER);
				sb.append(key);
				sb.append(DIVIDER);
				sb.append(((Float)value).doubleValue());
			}
		}
		
		
		return sb.toString();
	}
	
	public void load(IResource resource) {
	}
	public void store(IResource resource) {
	}
}
