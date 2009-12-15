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
package org.jboss.tools.common.model.impl;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.XModelImpl;

/**
 * This is a class for holding children of model object
 * which may be sorted by a specific comparator.
 * @author glory
 */
public class RegularChildren {
	protected static XModelObject[] EMPTY = new XModelObject[0];

	protected SMap objects = null;
	protected Comparator<XModelObject> comparator = null;

	public RegularChildren() {}
    
	/**
	 * Returns false if children may be sorted. Returns true if order of
	 * children is important (e.g. it is defined by storage).
	 * 
	 * @return
	 */
	public boolean areChildrenOrdered() {
		return false;
	}
    
	/**
	 * Removes all children. To save memory, object keeping children is set to
	 * null.
	 */
	public void clear() {
		objects = null;
	}

	/**
	 * Sets comparator to sort children if it is allowed.
	 * 
	 * @param c
	 */
	public void setComparator(Comparator<XModelObject> c) {
		comparator = c;
	}

	/**
	 * Returns true if there are no children.
	 * 
	 * @return
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/**
	 * Returns the amount of children.
	 * 
	 * @return
	 */
	public int size() {
		return (objects == null) ? 0 : objects.size();
	}
	
	/**
	 * Returns the amount of children for specified entity.
	 * 
	 * @param entity
	 * @return
	 */
	public int getChildrenCount(String entity) {
		return (objects == null) ? 0 : objects.getChildrenCount(entity);
	}
	
	/**
	 * returns sorted values
	 * 
	 * @return
	 */
	public XModelObject[] getObjects() {
		return (isEmpty()) ? EMPTY : objects.getSortedValues(comparator);
	}
    
	/**
	 * Returns copy of object map, where keys are path parts of objects.
	 * 
	 * @return
	 */
	public Map<String, XModelObject> getObjectsMap() {
		Map<String, XModelObject> result = new HashMap<String, XModelObject>();
		if (objects != null) {
			synchronized(objects) {
				result.putAll(objects.getMap());
			}
		}
		return result;
	}
    
	/**
	 * Returns object by key which should be equal to its path part.
	 * 
	 * @param key
	 * @return
	 */
	public XModelObject getObject(String key) {
		return (isEmpty()) ? null : objects.get(key);
	}

	/**
	 * Adds child object. If a child with the same path part exists the
	 * operation is rejected and false is returned.
	 * 
	 * @param o
	 * @return
	 */
	public boolean addObject(XModelObject o) {
		String pp = o.getPathPart();
		if (pp == null)
			return false;
		if (objects != null && objects.get(pp) != null) {
			if (objects.get(pp) == o)
				return false;
			((XModelObjectImpl) o).setParent_0(null);
			return false;
		}
		if (objects == null)
			objects = new SMap();
		objects.put(pp, o);
		return true;
	}
    
	/**
	 * Removes child object. To save memory, if children set becomes empty,
	 * object keeping children is set to null.
	 * 
	 * @param o
	 * @return
	 */
	public boolean removeObject(XModelObject o) {
		if (objects == null)
			return false;
		String s = o.getPathPart();
		if (objects.get(s) == null)
			return false;
		((XModelObjectImpl) o).setParent_0(null);
		objects.remove(s);
		if (objects.size() == 0)
			objects = null;
		return true;
	}

	/**
	 * Request for change of path part in specified child. If other object
	 * exists that has proposed new path part, change is rejected and that
	 * object is returned as cause for the failure. Otherwize, object is
	 * re-registered in children map with new key.
	 * 
	 * @param o
	 * @param opp
	 * @param npp
	 * @return
	 */
	public XModelObject change(XModelObject o, String opp, String npp) {
		if (opp != null && opp.equals(npp))
			return null;
		XModelObject c = getObject(npp);
		if (c != null && c != o)
			return c;
		if (objects == null)
			objects = new SMap();
		if (opp != null)
			objects.remove(opp);
		objects.put(npp, o);
		if (opp != null && o.getParent() != null) {
			XModelImpl m = (XModelImpl) o.getModel();
			m.fireStructureChanged(o.getParent());
		}
		return null;
	}
    
	/**
	 * Returns index of specified child in children array. If children can be
	 * sorted, index is computed by sorted array. If child does not belong to
	 * children, than -1 is returned.
	 * 
	 * @param o
	 * @return
	 */
	public int getIndex(XModelObject o) {
		XModelObject[] os = getObjects();
		for (int i = 0; i < os.length; i++)	if (os[i] == o)	return i;
		return -1;
	}

	/**
	 * Moves child from position 'from' to position 'to'. This method is not
	 * relevant for children that may be sorted by comparator. Returns true if
	 * change in the order of children did occure.
	 * 
	 * @param from
	 * @param to
	 * @return
	 */
	public boolean move(int from, int to) {
		return false;
	}

	public void replaceChildren(XModelObject[] objects) {
		if(objects.length == 0) {
			if(this.objects != null) this.objects = null;
		} else {
			SMap m = new SMap();
			for (int i = 0; i < objects.length; i++) {
				m.put(objects[i].getPathPart(), objects[i]);
			}
			this.objects = m;
		}
	}
	
}

/**
 * Keeps objects in a fast up-to-date map and in a sorted cache that is lazily
 * updated on requests.
 * 
 * @author glory
 */
class SMap {
	Map<String,XModelObject> entries = new HashMap<String, XModelObject>();
	XModelObject[] cache = RegularChildren.EMPTY;

	public int size() {
		return entries.size();
	}

	public XModelObject get(String key) {
		return entries.get(key);
	}

	public void put(String key, XModelObject value) {
		synchronized(entries) {
			entries.put(key, value);
		}
		if(cache != null) {
			synchronized(this) {
				cache = null;
			}
		}
	}
    
	/**
	 * Returns the stored map.
	 * 
	 * @return
	 */
	public Map<String, XModelObject> getMap() {
		return entries;
	}

	private XModelObject[] values() {
		synchronized(entries) {
			return entries.values().toArray(RegularChildren.EMPTY);
		}
	}

	/**
	 * Returns values of map sorted by the passed comparator.
	 * @param comparator
	 * @return
	 */
	public XModelObject[] getSortedValues(Comparator<XModelObject> comparator) {
		XModelObject[] c = cache;
		//Otherwise, cache can be made null between 'if' and 'return', but we 
		//avoid synchronizing this line for the most probable return to be very fast.
		if (c != null) return c;
		synchronized (this) {
			if (cache != null) return cache;
			if (size() == 0) {
				cache = RegularChildren.EMPTY;
			} else {
				cache = values();
				if (comparator != null)
					Arrays.sort(cache, comparator);
			}
			return cache;
		}
	}

	public void remove(String key) {
		synchronized(entries) {
			entries.remove(key);
		}
		if(cache != null) {
			synchronized(this) {
				cache = null;
			}
		}
	}

	/**
	 * Returns the amount of objects for specified entity.
	 * @param entity
	 * @return
	 */
	public int getChildrenCount(String entity) {
		int k = 0;
		synchronized(entries) {
			for (XModelObject r : entries.values()) {
				String e = r.getModelEntity().getName();
				if (entity.equals(e)) ++k;
			}
		}
		return k;
	}
	
}
