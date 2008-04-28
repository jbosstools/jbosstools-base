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
package org.jboss.tools.common.xml;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

class ErrorHandlerImpl implements ErrorHandler {
	  List<String> errors = new ArrayList<String>();

	  public void error(SAXParseException e) throws SAXException {
	      add(e);
	  }

	  public void fatalError(SAXParseException e) throws SAXException {
	      add(e);
	      throw e;
	  }

	  public void warning(SAXParseException e) throws SAXException {
	      add(e);
	  }

	  private void add(SAXParseException e) {
	      errors.add("" + e.getMessage() + ":" + e.getLineNumber() + ":" + e.getColumnNumber());
	  }

	}
