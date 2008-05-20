/*
 * XModelFactory.java
 *
 * Created on January 17, 2002, 9:36 AM
 */

package org.jboss.tools.common.model;

import java.util.*;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.util.*;

/**
 * Factory class to create instances of XModel interface.
 * See getDefaultInstance() method for details.
 * @author IShabalov
 */
public class XModelFactory {
    
    static public final String META_PATH = "/meta";
    
	private static XModel defaultInstance = null;
	private static Object monitor = new Object();
    
	public static XModel getModel(Properties p) {
		return createInstance(p);
	}
    
	private static final XModel createInstance(Properties p) {
		XModelConstants.validate(p);
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		XModelImpl model = new XModelImpl(p, meta);
		model.load();
		return model;
	}
    
	/** Use this method to obtain default instance of interface org.jboss.tools.common.model.XModel
	 * This implementation return single instance and create it if it was not created yet.
	 * @return default XModel instance.
	 *
	 */    
	public static final XModel getDefaultInstance() {
		if (defaultInstance != null) {
			return defaultInstance;
		} else {
			synchronized (monitor) {
				if (defaultInstance == null) {
					defaultInstance = createInstance(ClassLoaderUtil.getProperties());
				}
			}
			return defaultInstance;
		}
	}
}
