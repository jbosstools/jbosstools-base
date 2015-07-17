/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.jboss.tools.common.jdt.debug.VmModel;

public class VmModelCache {
	private static VmModelCache instance = new VmModelCache();
	public static VmModelCache getDefault() {
		return instance;
	}
	
	private HashMap<String, List<VmModel>> models;
	public VmModelCache() {
		models = new HashMap<String, List<VmModel>>();
	}
	
	public synchronized List<VmModel> getModels(String hostName) {
		return models.get(hostName);
	}

	public synchronized void cacheModels(String host, List<VmModel> models) {
		this.models.put(host, models);
	}
	public synchronized VmModel getModel(String hostName, int pid) {
		List<VmModel> list = models.get(hostName);
		if( list == null ) {
			return null;
		}
		Iterator<VmModel> i = list.iterator(); 
		while(i.hasNext()) {
			VmModel m = i.next();
			if( m.getPid().equals(Integer.toString(pid))) {
				return m;
			}
		}
		return null;
	}
	
	public synchronized void cacheModel(String hostName, int pid, VmModel m) {
		List<VmModel> list = models.get(hostName);
		if( list == null ) {
			list = new ArrayList<VmModel>();
			models.put(hostName, list);
		}
		Iterator<VmModel> i = list.iterator(); 
		while(i.hasNext()) {
			VmModel mo = i.next();
			if( mo.getPid().equals(Integer.toString(pid))) {
				i.remove();
			}
		}
		list.add(m);
	}
	
}
