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

import java.util.*;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.RecognizedFileImpl;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.model.util.XMLUtil;
import org.jboss.tools.common.xml.SAXValidator;

public class AbstractXMLFileImpl extends RecognizedFileImpl {
    private static final long serialVersionUID = 3794621010387468744L;
	private RM markers = new RM();
	protected String[] _errors = new String[0];
	protected String[] errors = new String[0];
	
	/**
	 * Licence does not allows for distributing copies of dtds.
	 */
	public final static boolean turnOffDTDCheck = true;
	
	public boolean isIncorrect() {
		return ("yes".equals(get("isIncorrect")));
	}

	public AbstractXMLFileImpl() {
		markers.setModelObject(this);
	}
	
	public String[] getErrors() {
		return _errors; 	
	}

	protected final void setErrors(String body, boolean checkDTD, boolean checkSchema) {
		String[] errors = (body.length() == 0) ? null
				: (checkSchema) ? new SAXValidator().getXMLErrors(new java.io.StringReader(body))
				: XMLUtil.getXMLErrors(new java.io.StringReader(body), checkDTD);
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
			String pos = er.substring(q + 1);
			er = er.substring(0, q);
			q = er.lastIndexOf(':');
			String ln = er.substring(q + 1), ln1 = ln;
			er = er.substring(0, q);
			int iln = -1;
			try {
				if(q >= 0 && ln1.length() > 0) {
					iln = Integer.parseInt(ln1);
					ln1 = "" + (iln - 1);
				}
			} catch (Exception e) {
				ModelPlugin.log(e);
			}
			String ep = "ERROR: " + FindObjectHelper.makeRef(getPath() + ":" + ln1, ln + ":" + pos) + " " + er;
			if(iln < 0) markers.lines.remove(ep);
			else markers.lines.put(ep, new Integer(iln));
			sb.append(ep).append('\n');
			this.errors[i] = ep;
		}
		String s = sb.toString();
		if(s.equals(get("errors"))) return;
		super.set("incorrectBody", body);
		set("errors", s);
		setAttributeValue("isIncorrect", (errors.length == 0) ? "no" : "yes");
		if(!isOverlapped())	markers.update();
		
	}
    
	protected boolean isOverlapped() {
		XModelObject p = getParent();
		while(p != null && !"true".equals(p.get("overlapped"))) p = p.getParent();
		return (p != null);
	}

	class RM extends ResourceMarkers {
		Map<String,Integer> lines = new HashMap<String,Integer>();
		public RM() {
			super(ResourceMarkers.TEXT_PROBLEM);
		}
		protected String[] getErrors() {
			if(!isIncorrect()) return new String[0];
			String es = (String)get("errors");
			if(es == null || es.length() == 0) return new String[0]; 
			return errors;
		}
		protected int getLocation(String s) {
			Integer i = (Integer)lines.get(s);
			return (i == null) ? -1 : i.intValue();
		}		
	}
	
	protected ResourceMarkers getResourceMarkers() {
		return markers;
	}

}
