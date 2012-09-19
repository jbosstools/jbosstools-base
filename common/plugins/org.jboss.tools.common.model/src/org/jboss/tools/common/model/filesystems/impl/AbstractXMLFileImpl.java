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
package org.jboss.tools.common.model.filesystems.impl;

import java.text.MessageFormat;
import java.util.*;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.RecognizedFileImpl;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.model.util.XMLUtil;

public class AbstractXMLFileImpl extends RecognizedFileImpl {
    private static final long serialVersionUID = 3794621010387468744L;
	private RM markers = new RM();
	protected String[] _errors = new String[0];
	protected String[] errors = new String[0];
	
	protected String loaderError = null;
	
	/**
	 * Licence does not allows for distributing copies of dtds.
	 */
	public final static boolean turnOffDTDCheck = true;
	
	public boolean isIncorrect() {
		return (YES.equals(get(ATTR_NAME_IS_INCORRECT)));
	}

	public AbstractXMLFileImpl() {
		markers.setModelObject(this);
	}
	
	public String[] getErrors() {
		return _errors; 	
	}
	
	public void setLoaderError(String loaderError) {
		this.loaderError = loaderError;
	}
	
	public String getLoaderError() {
		return loaderError;
	}

	protected final void setErrors(String body, boolean checkDTD, boolean checkSchema) {
		String[] errors = (body.length() == 0) ? null 
				//do not compute errors for unrecognized files.
			: "FileXML".equals(getModelEntity().getName()) ? null //$NON-NLS-1$
				//do not compute errors for files in jars
			: (getParent() instanceof JarFolderImpl) ? null 
			: XMLUtil.getXMLErrors(new java.io.StringReader(body), checkDTD, checkSchema);
		if(errors == null || errors.length == 0) {
			if(loaderError != null) errors = new String[]{loaderError};
		}
		setErrors(body, errors);
	}
	
	protected final void setErrors(String body, String[] errors) {
		if(errors == null) errors = new String[0];
		this.errors = errors;
		_errors = (String[])errors.clone();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < errors.length; i++) {
			String er = errors[i];
			int q = er.lastIndexOf(':');
			String pos = (q < 0) ? "" : er.substring(q + 1); //$NON-NLS-1$
			if(q >= 0) er = er.substring(0, q);
			q = er.lastIndexOf(':');
			String ln = (q < 0) ? "" : er.substring(q + 1), ln1 = ln; //$NON-NLS-1$
			if(q >= 0) er = er.substring(0, q);
			int iln = -1;
			try {
				if(q >= 0 && ln1.length() > 0) {
					iln = Integer.parseInt(ln1);
					ln1 = "" + (iln - 1); //$NON-NLS-1$
				}
			} catch (NumberFormatException e) {
//				ignore, some errors found by entity resolver do not have coordinates
//				        but use similar format to mention io exception.  
//				ModelPlugin.getPluginLog().logError(e);
			}
			String ep = MessageFormat.format("ERROR: {0} {1}", FindObjectHelper.makeRef(getPath() + ":" + ln1, ln + ":" + pos), er);  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			if(iln < 0) markers.lines.remove(ep);
			else markers.lines.put(ep, Integer.valueOf(iln));
			sb.append(ep).append('\n');
			this.errors[i] = ep;
		}
		String s = sb.toString();
		if(s.equals(get("errors"))) return; //$NON-NLS-1$
		super.set(ATTR_NAME_INCORRECT_BODY, (errors.length == 0 && loaderError == null) ? "" : body); //$NON-NLS-1$
		set("errors", s); //$NON-NLS-1$
		setAttributeValue(ATTR_NAME_IS_INCORRECT, (errors.length == 0 && loaderError == null) ? NO : YES);
		if(!isOverlapped())	markers.update();
		
	}
    
	protected boolean isOverlapped() {
		XModelObject p = getParent();
		while(p != null && !TRUE.equals(p.get("overlapped"))) p = p.getParent(); //$NON-NLS-1$
		return (p != null);
	}

	class RM extends ResourceMarkers {
		Map<String,Integer> lines = new HashMap<String,Integer>();
		public RM() {
			super(ResourceMarkers.TEXT_PROBLEM);
		}
		protected String[] getErrors() {
			if(!isIncorrect()) return new String[0];
			String es = (String)get("errors"); //$NON-NLS-1$
			if(es == null || es.length() == 0) return new String[0]; 
			return errors;
		}
		protected int getLocation(String s) {
			Integer i = (Integer)lines.get(s);
			return (i == null) ? -1 : i.intValue();
		}
		
		boolean enabled = false;
		public void update() {
			if(enabled) super.update(); 
		}
	}
	
	protected ResourceMarkers getResourceMarkers() {
		return markers;
	}

}
