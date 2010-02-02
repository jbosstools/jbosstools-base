/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.loaders;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class EntityRecognizerContext {
	protected String fileName;
	protected String extension;
	protected String body;
	protected XMLRecognizerContext xml = null;

	public EntityRecognizerContext(String extension) {
		this.extension = extension;
	}

	public EntityRecognizerContext(String extension, String body) {
		this.extension = extension;
		this.body = body;
	}

	public EntityRecognizerContext(String fileName, String extension, String body) {
		this.fileName = fileName;
		this.extension = extension;
		this.body = body;
	}

	public String getFileName() {
		return fileName;
	}

	public String getExtension() {
		return extension;
	}

	public String getBody() {
		return body;
	}

	public XMLRecognizerContext getXMLContext() {
		if(xml == null) {
			xml = new XMLRecognizerContext(this);
		}
		return xml;
	}

}
