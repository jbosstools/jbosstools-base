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
package org.jboss.tools.common.meta.ui.search;

import java.util.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.search.ui.*;

public class MetaSearchResult implements ISearchResult {
	Set listeners = new HashSet();
	ISearchQuery query;
	List objects = new ArrayList();
	long timeStamp = 0;
	
	public void setQuery(ISearchQuery query) {
		this.query = query;
	}

	public void addListener(ISearchResultListener l) {
		listeners.add(l);		
	}

	public void removeListener(ISearchResultListener l) {
		listeners.remove(l);
	}

	public String getLabel() {
		if(query instanceof MetaSearchQuery) {
			MetaSearchQuery q = (MetaSearchQuery)query;
			return "Meta Search - '" + q.getTextToFind() + "' - " + objects.size() + " matches";
		}
		return "Meta Search";
	}

	public String getTooltip() {
		return "Meta Search";
	}

	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	public ISearchQuery getQuery() {
		return query;
	}
	
	public void removeAll() {
		objects.clear();
	}
	
	public void addObject(Object object) {
		objects.add(object);
		timeStamp++;
		fire();
	}
	
	void fire() {
		ISearchResultListener[] ls = (ISearchResultListener[])listeners.toArray(new ISearchResultListener[0]);
		for (int i = 0; i < ls.length; i++) ls[i].searchResultChanged(new SearchResultEvent(this){});
	}
	
	public List getObjects() {
		return objects;
	}
	
	public long getTimeStamp() {
		return timeStamp;
	}

}
