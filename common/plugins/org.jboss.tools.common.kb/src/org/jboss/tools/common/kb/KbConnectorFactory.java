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

import java.util.HashMap;

/**
 * @author eskimo
 */
public class KbConnectorFactory {

    private HashMap<Object,Object> connectorInstances;

    private KbConnectorFactory() {
        connectorInstances = new HashMap<Object,Object>();
	}

    /**
     * 
     * @param type
     * @return
     * @throws ClassNotFoundException
     * @throws InstantiationException
     * @throws IllegalAccessException
     */
	public KbConnector createConnector(KbConnectorType type) throws ClassNotFoundException, InstantiationException, IllegalAccessException{
	    if(type.isSingleton()) {
	        Object o = connectorInstances.get(type.toString());
	        if(o!=null) {
	            return (KbConnector)o;
	        }
	    }
		Object newInstance = type.getConnectorClass().newInstance();
	    connectorInstances.put(type.toString(), newInstance);
		return (KbConnector) newInstance; 
	}

	/**
	 * 
	 * @param type
	 * @param loader
	 * @return
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws ClassNotFoundException
	 */
	public KbConnector createConnector(KbConnectorType type, ClassLoader loader) throws InstantiationException, IllegalAccessException, ClassNotFoundException {
	    if(type.isSingleton()) {
	        Object o = connectorInstances.get(type.toString());
	        if(o!=null) {
	            return (KbConnector)o;
	        }
	    }
	    String className = type.getConnectorClass().getName();
		Class classInstance = Class.forName(className,true,loader);
		Object newInstance = classInstance.newInstance(); 
	    connectorInstances.put(type.toString(), newInstance);
		return (KbConnector) newInstance;
	}

	/**
	 * 
	 * @param type
	 * @param key
	 * @return
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws ClassNotFoundException
	 */
	public KbConnector createConnector(KbConnectorType type, Object key) throws InstantiationException, IllegalAccessException, ClassNotFoundException {
	    Object realKey = key;
	    if(type.isSingleton()) {
	        Object o = connectorInstances.get(type.toString());
	        if(o!=null) {
	            return (KbConnector)o;
	        } else {
	            realKey = type.toString();
	        }
	    }
        Object o = connectorInstances.get(realKey);
        if(o!=null) {
            return (KbConnector)o;
        }
		Object newInstance = type.getConnectorClass().newInstance();
	    connectorInstances.put(realKey, newInstance);
		return (KbConnector) newInstance; 
	}

	/**
	 * 
	 * @return
	 */
	static public KbConnectorFactory getIntstance() {
		return KbConnectionFactoryHolder.INSTANCE;
	}

	static class KbConnectionFactoryHolder {
		static final KbConnectorFactory INSTANCE = new KbConnectorFactory();
	}
}