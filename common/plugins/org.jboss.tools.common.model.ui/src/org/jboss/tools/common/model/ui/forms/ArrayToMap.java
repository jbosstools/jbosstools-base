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

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author Igels
 *
 */
public class ArrayToMap implements Map<String,IFormData> {
	Map<String,IFormData> map = new HashMap<String,IFormData>();

	public ArrayToMap(IFormData[] formsData) {
		for(int i = 0; i < formsData.length; i++) {
			map.put(formsData[i].getEntityName(), formsData[i]);
		}
	}

	public void clear() {
		map.clear();
	}

	public boolean containsKey(Object key) {
		return map.containsKey(key);
	}

	public boolean containsValue(Object value) {
		return map.containsValue(value);
	}

	public Set<Map.Entry<String, IFormData>> entrySet() {
		return map.entrySet();
	}

	public boolean equals(Object obj) {
		return map.equals(obj);
	}

	public IFormData get(Object key) {
		return map.get(key);
	}

	public int hashCode() {
		return map.hashCode();
	}

	public boolean isEmpty() {
		return map.isEmpty();
	}

	public Set<String> keySet() {
		return map.keySet();
	}

	public IFormData put(String key, IFormData value) {
		return map.put(key, value);
	}

	public void putAll(Map t) {
		Iterator it = t.keySet().iterator();
		while(it.hasNext()) {
			Object key = it.next();
			if(!(key instanceof String)) continue;
			String skey = key.toString();
			Object value = t.get(key);
			if(!(value instanceof IFormData)) continue;
			IFormData f = (IFormData)value;
			map.put(skey, f);
		}
	}

	public IFormData remove(Object key) {
		return map.remove(key);
	}

	public int size() {
		return map.size();
	}

	public String toString() {
		return map.toString();
	}

	public Collection<IFormData> values() {
		return map.values();
	}
}