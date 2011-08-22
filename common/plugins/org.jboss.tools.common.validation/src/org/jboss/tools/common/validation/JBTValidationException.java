/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

/**
 * Presents an exception that can be thrown during KB validation.
 * 
 * @author Alexey Kazakov
 */
public class JBTValidationException extends Exception {

	private static final long serialVersionUID = -1958111192543021067L;

    public JBTValidationException(String message, Throwable cause) {
        super(message, cause);
    }
}