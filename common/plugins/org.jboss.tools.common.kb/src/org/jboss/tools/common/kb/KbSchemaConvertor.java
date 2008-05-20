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
package org.jboss.tools.common.kb;

import java.io.File;
import java.io.InputStream;

import org.w3c.dom.Document;

/**
 * Class converts Resource to Schema
 * @author igels
 */
public interface KbSchemaConvertor {

	/**
	 * 
	 * @param inputStream
	 * @return
	 */
	public Document convertToSchema(InputStream inputStream);

	/**
	 * 
	 * @param resource
	 * @return
	 */
	public Document convertToSchema(KbResource resource);

	/**
	 * 
	 * @param sourceFile
	 * @return
	 */
	public Document convertToSchema(File sourceFile);
}
