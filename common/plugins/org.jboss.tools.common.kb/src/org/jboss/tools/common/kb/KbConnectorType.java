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

import org.jboss.tools.common.kb.wtp.JspWtpKbConnector;

/**
 * @author eskimo
 */
public abstract class KbConnectorType {

    protected KbConnectorType() {
	}

    /**
     * 
     * @return
     */
    public abstract Class getConnectorClass();

    /**
     * 
     * @return
     */
    public boolean isSingleton() {
        return false;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return getConnectorClass().getName();
    }

    /**
     * 
     */
    public static KbConnectorType XML_KB_CONNECTOR = 
		new KbConnectorType() {
			public Class getConnectorClass() {
				return XmlKbConnector.class;			 	
			}
		};

	/**
	 * 
	 */
	public static KbConnectorType JSP_KB_CONNECTOR = 
		new KbConnectorType() {
			public Class getConnectorClass() {
				return JspKbConnector.class;			 	
			}
		};

	/**
	 * 
	 */
	public static KbConnectorType JSP_WTP_KB_CONNECTOR = 
		new KbConnectorType() {
			public Class getConnectorClass() {
				return JspWtpKbConnector.class;			 	
			}
		};
}