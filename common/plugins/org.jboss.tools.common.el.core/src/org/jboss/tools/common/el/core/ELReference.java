/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.parser.SyntaxError;
import org.jboss.tools.common.text.ITextSourceReference;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.common.util.UniquePaths;
import org.w3c.dom.Element;

/**
 * Represents a reference to EL in a resource
 * @author Alexey Kazakov
 */
public class ELReference implements ITextSourceReference {
	private IPath path;
	private int length;
	private int lineNumber;
	private int startPosition;
	private Set<IMarker> markers;
	private boolean needToInitMarkers = false;
	private String source;
	private String elMarkerGroupID;
	
	public ELReference() {
		
	}
	
	public ELReference(String elMarkerGroupID) {
		this.elMarkerGroupID = elMarkerGroupID;
	}
	
	/**
	 * @return
	 */
	public int getLineNumber() {
		return lineNumber;
	}

	/**
	 * @param lineNumber the lineNumber to set
	 */
	public void setLineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.core.ISeamTextSourceReference#getLength()
	 */
	public int getLength() {
		return length;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.core.ISeamTextSourceReference#getStartPosition()
	 */
	public int getStartPosition() {
		return startPosition;
	}

	/**
	 * @param length
	 */
	public void setLength(int length) {
		this.length = length;
	}

	/**
	 * @param startPosition
	 */
	public void setStartPosition(int startPosition) {
		this.startPosition = startPosition;
	}

	/**
	 * @return the resource
	 */
	public IFile getResource() {
		return ResourcesPlugin.getWorkspace().getRoot().getFile(path);
	}

	/**
	 * @param resource the resource to set
	 */
	public void setResource(IFile resource) {
		if(resource != null) {
			this.path = UniquePaths.getInstance().intern(resource.getFullPath());
		}
	}

	/**
	 * @return the path
	 */
	public IPath getPath() {
		return path;
	}

	/**
	 * @param path the path to set
	 */
	public void setPath(IPath path) {
		this.path = UniquePaths.getInstance().intern(path);
	}

	public String getSourceText() {
		if(source == null) {
			source = getText();// getELModel().getSource();
		}
		return source;
	}

	public int getStartPossitionOfFirstEL() {
		ELExpression[] els = getEl();
		if(els.length>0) {
			return startPosition + els[0].getStartPosition();
		}
		return startPosition;
	}

	/**
	 * Helper method, text should be the segment of file content determined by startPosition and length.
	 * Check is not done, because it would affect performance. 
	 * 
	 * @param text
	 */
	public ELExpression[] init(String text) {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		ELModel model = parser.parse(text);
		return setEl(model.getInstances());
	}

	/**
	 * @return the el
	 */
	public ELExpression[] getEl() {
		ELExpression[] el = null;
			String text = getSourceText();
			if(text.length() > 0) {
				el = init(text);
			} else {
				el = new ELExpression[0];
			}
		return el;
	}

	private String getText() {
		String text = FileUtil.getContentFromEditorOrFile(getResource());
		if(text != null && getStartPosition() >= 0 && getLength() >= 0 && text.length() >= getStartPosition() + getLength()) {
			return source = "" + text.substring(getStartPosition(), getStartPosition() + getLength());
		} else {
			return source = "";
		}
		
	}

	/**
	 * @param insts
	 */
	public ELExpression[] setEl(List<ELInstance> insts) {
		Set<ELExpression> exps = new HashSet<ELExpression>();
		for (ELInstance el : insts) {
			exps.add(el.getExpression());
		}
		return exps.toArray(new ELExpression[0]);
	}

	private static final IMarker[] EMPTY_MARKER_ARRAY = new IMarker[0];

	private void initMarkers() {
		if(markers == null && needToInitMarkers) {
			IFile file = getResource();
			if(file!=null) {
				IMarker[] markers = null;
				try {
					markers = file.findMarkers(null, true, IResource.DEPTH_INFINITE);
				} catch (CoreException e) {
					ELCorePlugin.getDefault().logError(e);
				}
				for(int i=0; i<markers.length; i++){
					String groupName = markers[i].getAttribute("groupName", null); //$NON-NLS-1$
					if(groupName!=null && (groupName.equals(this.elMarkerGroupID))) {
						int start = markers[i].getAttribute(IMarker.CHAR_START, -1);
						int end = markers[i].getAttribute(IMarker.CHAR_END, -1);
						if(start>=startPosition && end<=startPosition+length) {
							addMarker(markers[i]);
						}
					}
				}
			}
			needToInitMarkers = false;
		}
	}

	/**
	 * @return the syntaxErrors
	 */
	public List<SyntaxError> getSyntaxErrors() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String text = getSourceText();
		if(text.length() == 0) return Collections.emptyList();
		ELModel model = parser.parse(text);
		return model.getSyntaxErrors();
	}


	public String getMarkerGroupId() {
		return this.elMarkerGroupID;
	}

	/**
	 * @param needToInitMarkers the needToInitMarkers to set
	 */
	public synchronized void setNeedToInitMarkers(boolean needToInitMarkers) {
		this.needToInitMarkers = needToInitMarkers;
	}

	/**
	 * @param markers the markers to set
	 */
	public synchronized void addMarker(IMarker marker) {
		if(marker==null) {
			return;
		}
		if(markers==null) {
			markers = new HashSet<IMarker>();
		}
		markers.add(marker);
	}

	/**
	 * Removes all markers from this EL.
	 */
	public void deleteMarkers() {
		Set<IMarker> aMarkers = null;
		synchronized (this) {
			initMarkers();
			if(markers == null) {
				return;
			}
			aMarkers = markers;
			markers = null;
		}

		for (IMarker marker : aMarkers) {
			try {
				marker.delete();
			} catch (CoreException e) {
				ELCorePlugin.getDefault().logError(e);
			}
		}
	}

	/**
	 * Store this EL into XML element.
	 * @param element
	 */
	public synchronized void store(Element element, Map<String,String> pathIds) {
		element.setAttribute("path", getAlias(pathIds, path.toString())); //$NON-NLS-1$
		element.setAttribute("offset", Integer.toString(startPosition)); //$NON-NLS-1$
		element.setAttribute("length", Integer.toString(length)); //$NON-NLS-1$
	}

	public static String getAlias(Map<String, String> pathAliases, String path) {
		String result = pathAliases.get(path);
		if(result == null) {
			result = "%" + pathAliases.size(); //$NON-NLS-1$
			pathAliases.put(path, result);
		}
		return result;
	}

	public static String getPath(Map<String, String> pathAliases, String alias) {
		return pathAliases.containsKey(alias) ? pathAliases.get(alias) : alias;
	}

	/**
	 * Load this EL from XML element.
	 * @param element
	 */
	public synchronized void load(Element element, Map<String, String> pathAliases) {
		path = new Path(getPath(pathAliases, element.getAttribute("path"))); //$NON-NLS-1$
		setPath(path);
		startPosition = new Integer(element.getAttribute("offset")); //$NON-NLS-1$
		length = new Integer(element.getAttribute("length")); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(this == obj) {
			return true;
		}
		ELReference el = (ELReference)obj;
		return this.path.equals(el.path) && this.startPosition == el.startPosition;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return path.hashCode() + startPosition;
	}

	public ELModel getELModel() {
		ELExpression[] exprs = getEl();
		if(exprs.length>0) {
			return exprs[0].getModel();
		}
		return null;
	}
}