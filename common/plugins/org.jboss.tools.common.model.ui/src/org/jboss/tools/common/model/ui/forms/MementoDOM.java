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
import java.util.Hashtable;

import org.eclipse.ui.IMemento;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author AU
 */
public class MementoDOM implements IMemento {
	
	private Element element;
	private static final String ID = "id"; //$NON-NLS-1$
	private static final String MEMENTO = "Memento"; //$NON-NLS-1$
	private static final String TYPE = "type"; //$NON-NLS-1$
	private Hashtable hash = new Hashtable();
	
	private MementoDOM() {}
	
	private MementoDOM(Element owner, String type) {
		element = owner.getOwnerDocument().createElement(MEMENTO);
		element.setAttribute(TYPE, type);
		owner.appendChild(element);
	}

	public MementoDOM(Node node) {
		this.element = (Element)node;
	}

	private IMemento getMemento(Node node) {
		if (node==null) return null;
		IMemento memento = (IMemento)hash.get(node);
		if (memento==null) {
			memento = new MementoDOM(node);
			hash.put(node, memento); 
		}
		return memento;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#createChild(java.lang.String)
	 */
	public IMemento createChild(String type) {
		return new MementoDOM(element, type);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#createChild(java.lang.String, java.lang.String)
	 */
	public IMemento createChild(String type, String id) {
		MementoDOM newMemento = new MementoDOM(element, type);
		newMemento.setID(id);
		return newMemento;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getChild(java.lang.String)
	 */
	public IMemento getChild(String type) {
		NodeList nl = element.getChildNodes();
		Node node;
		for (int i=0; i<nl.getLength(); ++i) {
			node = nl.item(i);
			if ((node instanceof Element) 
				&& (type.equals(((Element)node).getAttribute(TYPE)))) return getMemento(node); 
		} 
		return null; 
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getChildren(java.lang.String)
	 */
	public IMemento[] getChildren(String type) {
		NodeList nl = element.getElementsByTagName(type);
		ArrayList list = new ArrayList();
		Node node;
		for (int i=0; i<nl.getLength(); ++i) {
			node = nl.item(i);
			if ((node instanceof Element) 
				&& (type.equals(((Element)node).getAttribute(TYPE)))) list.add(getMemento(node)); 
			
		} 
		return (IMemento[])list.toArray(new IMemento[list.size()]);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getFloat(java.lang.String)
	 */
	public Float getFloat(String key) {
		return new Float(element.getAttribute(key));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getID()
	 */
	public String getID() {
		return element.getAttribute(ID);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getInteger(java.lang.String)
	 */
	public Integer getInteger(String key) {
		return Integer.valueOf(element.getAttribute(key));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getString(java.lang.String)
	 */
	public String getString(String key) {
		return element.getAttribute(key);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#getTextData()
	 */
	public String getTextData() {
		return element.getNodeValue();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putFloat(java.lang.String, float)
	 */
	public void putFloat(String key, float value) {
		element.setAttribute(key, ""+value); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putInteger(java.lang.String, int)
	 */
	public void putInteger(String key, int value) {
		element.setAttribute(key, ""+value); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putMemento(org.eclipse.ui.IMemento)
	 */
	public void putMemento(IMemento memento) {
		element.appendChild(((MementoDOM)memento).getElement());
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putString(java.lang.String, java.lang.String)
	 */
	public void putString(String key, String value) {
		element.setAttribute(key, value);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IMemento#putTextData(java.lang.String)
	 */
	public void putTextData(String data) {
		element.setNodeValue(data);
	}

	public void setID(String id) {
		element.setAttribute(ID, id);
	}

	private Element getElement() {
		return element;
	}
	
	public String[] getAttributeKeys() {
		NamedNodeMap map = element.getAttributes();
		int size = map.getLength();
		String[] attributes = new String[size];
		for (int i = 0; i < size; i++) {
			Node node = map.item(i);
			attributes[i] = node.getNodeName();
		}
		return attributes;

	}

	public Boolean getBoolean(String key) {
        String attr = getString(key);
        if (attr == null) {
			return null;
		}
        return Boolean.valueOf(attr);
	}
   
	public void putBoolean(String key, boolean value) {
		putString(key, value ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public String getType() {
		return getString(TYPE);
	}
	
	
}
