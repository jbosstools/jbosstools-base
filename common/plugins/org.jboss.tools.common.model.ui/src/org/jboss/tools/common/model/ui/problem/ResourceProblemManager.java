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
package org.jboss.tools.common.model.ui.problem;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.texteditor.MarkerUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author Aleksey
 */
public class ResourceProblemManager extends ProblemManager {
	private IResource resource;
	private Map<Problem,IMarker> problemsCache;

	public ResourceProblemManager() {}

	public ResourceProblemManager(IResource resource) {
		this.resource = resource;
	}

	public Problem addProblem(Problem problem) {
		problem.setLocation(resource.getLocation().toOSString());
		super.addProblem(problem);
		return problem;
	}
	
	public void cache() {
		problemsCache = new HashMap<Problem,IMarker>();
		IMarker[] ms = new IMarker[0];
		try {
			ms = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		for (int i = 0; i < ms.length; i++) {
			Problem p = getProblem(ms[i]);
			int k = problems.indexOf(p);
			if(k >= 0) p = (Problem)problems.get(k);
			problemsCache.put(p, ms[i]);
		}
		problems.clear();
	}
	
	public void commit() {
		Problem[] ps = problems.toArray(new Problem[0]);
		for (int i = 0; i < ps.length; i++) {
			if(problemsCache.containsKey(ps[i])) {
				IMarker m = problemsCache.remove(ps[i]);
				int pos_m = m.getAttribute(IMarker.CHAR_START, Problem.NONE);
				int pos_i = ps[i].getPosition();
				if(pos_i != pos_m) {
					try {
						m.setAttribute(IMarker.CHAR_START, pos_i);
						m.setAttribute(IMarker.CHAR_END, pos_i + 1);
					} catch (Exception e) {
						ModelUIPlugin.getPluginLog().logError(e);
					}
				}
			} else {
				addMarker(ps[i]);
			}
		}
		IMarker[] ms = problemsCache.values().toArray(new IMarker[0]);
		for (int i = 0; i < ms.length; i++) {
			try {
				ms[i].delete();
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
	}

	private void addMarker(Problem problem) {
		HashMap<String,Object> markerData = new HashMap<String,Object>();
		markerData.put(IMarker.MESSAGE, problem.getMessage());
		markerData.put(IMarker.LOCATION, resource.getLocation().toOSString());
		markerData.put(IMarker.LINE_NUMBER, new Integer(problem.getLine()));
		markerData.put(IMarker.SEVERITY, new Integer(IMarker.SEVERITY_ERROR));
		int position = problem.getPosition();
		if(position != Problem.NONE) {
			markerData.put(IMarker.CHAR_START, new Integer(position));
			markerData.put(IMarker.CHAR_END, new Integer(position + 1));
		}		
		this.addMarker(markerData);
	}
	
	private Problem getProblem(IMarker marker) {
		String message = marker.getAttribute(IMarker.MESSAGE, "");
		String location = marker.getAttribute(IMarker.LOCATION, "");
		int line = marker.getAttribute(IMarker.LINE_NUMBER, -1);
		int position = marker.getAttribute(IMarker.CHAR_START, -1);
		String type = IMarker.PROBLEM;
		try { type = marker.getType(); } catch (Exception e) {}
		Problem p = new Problem(message, position, line, -1);
		p.setType(type);
		p.setLocation(location);
		return p;
	}

	private void addMarker(HashMap<String,Object> markerData) {
		try {
			MarkerUtilities.createMarker(resource, markerData, IMarker.PROBLEM);
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

	public IResource getResource() {
		return resource;
	}

	public void setResource(IResource resource) {
		this.resource = resource;
	}

}
