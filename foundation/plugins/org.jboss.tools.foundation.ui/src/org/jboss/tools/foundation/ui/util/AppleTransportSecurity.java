/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;

/**
 * A class that allows to query and set the permission for unsecure http
 * connections on MacOS. These are typically set in the file Info.plist of an
 * application bundle as follows:
 * 
 * <pre>{@code 
 * <key>NSAppTransportSecurity</key>
 * <dict>
 * 		<key>NSExceptionDomains</key>
 * 		<dict>
 * 			<key>localhost</key>
 * 			<dict>
 *				<key>NSTemporaryExceptionAllowsInsecureHTTPLoads</key>
 *				<true/>
 *			</dict>
 *		</dict>
 * </dict>
 * }</pre>
 * 
 * This class allows to set this at runtime (if you have no access to the Info.plist in your application bundle. 
 */
public class AppleTransportSecurity {

	private static final String CLASS_NAME_ID = "org.eclipse.swt.internal.cocoa.id";
	private static final String CLASS_NAME_NSNUMBER = "org.eclipse.swt.internal.cocoa.NSNumber";
	private static final String CLASS_NAME_NSSTRING = "org.eclipse.swt.internal.cocoa.NSString";
	private static final String CLASS_NAME_NSBUNDLE = "org.eclipse.swt.internal.cocoa.NSBundle";
	private static final String CLASS_NAME_NSDICTIONARY = "org.eclipse.swt.internal.cocoa.NSDictionary";

	private static final String KEY_APP_TRANSPORT_SECURITY = "NSAppTransportSecurity";
	private static final String KEY_EXCEPTION_DOMAINS = "NSExceptionDomains";
	private static final String KEY_TEMPORARY_EXCEPTION_ALLOWS_INSECURE_HTTP_LOADS = "NSTemporaryExceptionAllowsInsecureHTTPLoads";

	/**
	 * Returns {@code true} if http connections are allow in the apple transport security.
	 * 
	 * @return true if http connections are allowed
	 */
	public boolean isHttpAllowed(String host) {
		if (!isMac()) {
			return true;
		}
		try {
			Object transportSecurity = getAppTransportSecurity(getMainBundleDictionary());
			if (transportSecurity == null) {
				return false;
			}
			Object exceptionDomains = getExceptionDomains(transportSecurity);
			if (exceptionDomains == null) {
				return false;
			}
			Object hostException = getHostExceptionIn(host, exceptionDomains);
			if (hostException == null) {
				return false;
			}
			Object temporaryException = getTemporaryExceptionIn(hostException);
			if (Boolean.TRUE.equals(temporaryException)) {
				return true;
			}
			return false;
		} catch (ClassNotFoundException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException | InstantiationException Øe) {
			return false;
		}
	}

	/**
	 * Allows http connections in the Apple Transport Security. 
	 */
	public void allowHttp(String host) {
		if (!isMac()) {
			return;
		}
		Object hostException = null;
		Object exceptionDomains = null;
		Object mainBundle = null;
		Object transportSecurity = null;
		try {
			mainBundle = getMainBundleDictionary();
			if (mainBundle == null) {
				FoundationUIPlugin.pluginLog().logError("Could not allow http connections: main bundle not found");
				return;
			}
			transportSecurity = getAppTransportSecurity(mainBundle);
			if (transportSecurity != null) {
				exceptionDomains = getExceptionDomains(transportSecurity);
				if (exceptionDomains != null) {
					hostException = getHostExceptionIn(host, exceptionDomains);
				}
			}

			Object temporaryException = createTemporaryException();

			if (hostException == null) {
				hostException = createHostExceptionWith(host,temporaryException);
			} else {
				setTemporaryExceptionIn(hostException);
			}

			if (exceptionDomains == null) {
				exceptionDomains = createExceptionDomainsWith(hostException);
			} else {
				setHostExceptionIn(host, hostException, exceptionDomains);
			}

			if (transportSecurity == null) {
				transportSecurity = createAppTransportSecurityWith(exceptionDomains);
			} else {
				setAppExceptionDomainsIn(exceptionDomains, transportSecurity);
			}
			setAppExceptionDomainsIn(exceptionDomains, mainBundle);
		} catch (ClassNotFoundException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException | InstantiationException e) {
			FoundationUIPlugin.pluginLog().logError("Could not allow http connections.", e);
		}
	}

	private boolean isMac() {
		return Platform.OS_MACOSX.equals(Platform.getOS());
	}

	private Object getExceptionDomains(Object transportSecurityDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, InstantiationException, IllegalArgumentException {
		return getDictionaryValue(KEY_EXCEPTION_DOMAINS, transportSecurityDict);
	}

	private Object createExceptionDomainsWith(Object hostExceptionDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		return createDictionary(KEY_EXCEPTION_DOMAINS, hostExceptionDict);
	}
	
	private Object getHostExceptionIn(String host, Object exceptionDomainsDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, InstantiationException, IllegalArgumentException {
		return getDictionaryValue(host, exceptionDomainsDict);
	}

	private Object setHostExceptionIn(String host, Object hostException, Object exceptionDomainsDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		return setDictionaryValue(host, hostException, exceptionDomainsDict);
	}

	private Object createHostExceptionWith(String host, Object temporaryExceptionDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		return createDictionary(host, temporaryExceptionDict);
	}

	private Object getTemporaryExceptionIn(Object allowsArbitraryLoads)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, InstantiationException, IllegalArgumentException {
		return getBooleanValue(KEY_TEMPORARY_EXCEPTION_ALLOWS_INSECURE_HTTP_LOADS, allowsArbitraryLoads);
	}

	private void setTemporaryExceptionIn(Object hostExceptionDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		setDictionaryValue(KEY_TEMPORARY_EXCEPTION_ALLOWS_INSECURE_HTTP_LOADS, createNSNumber(true), hostExceptionDict);
	}

	private Object createTemporaryException()
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException, ClassNotFoundException {
		return createDictionary(KEY_TEMPORARY_EXCEPTION_ALLOWS_INSECURE_HTTP_LOADS, createNSNumber(true));
	}

	private Object getMainBundleDictionary()
			throws ClassNotFoundException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object mainBundle = Class.forName(CLASS_NAME_NSBUNDLE).getMethod("mainBundle").invoke(null);
		return Class.forName(CLASS_NAME_NSBUNDLE).getDeclaredMethod("infoDictionary").invoke(mainBundle);
	}

	private Object createDictionary(String key, Object value)
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException,
			SecurityException, ClassNotFoundException {
		Class<?> idClass = Class.forName(CLASS_NAME_ID);
		Method dictionaryWithObjectMethod = Class.forName(CLASS_NAME_NSDICTIONARY).getMethod("dictionaryWithObject", idClass, idClass);
		Object nsString = createNSString(key);
		return dictionaryWithObjectMethod.invoke(null, value, nsString);
	}

	private Object getAppTransportSecurity(Object mainBundleDict)
			throws ClassNotFoundException, IllegalAccessException, InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalArgumentException {
		return getDictionaryValue(KEY_APP_TRANSPORT_SECURITY, mainBundleDict);
	}

	private Object createAppTransportSecurityWith(Object exceptionDomainsDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		return createDictionary(KEY_APP_TRANSPORT_SECURITY, exceptionDomainsDict);
	}

	private Object setAppExceptionDomainsIn(Object exceptionDomainsDict, Object mainBundleDict)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		return setDictionaryValue(KEY_APP_TRANSPORT_SECURITY, exceptionDomainsDict, mainBundleDict);
	}

	private Object setDictionaryValue(String key, Object value, Object dictionary)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		Class<?> idClass = Class.forName(CLASS_NAME_ID);
		Class<?> nsStringClass = Class.forName(CLASS_NAME_NSSTRING);
		Class<?> nsDictionaryClass = Class.forName(CLASS_NAME_NSDICTIONARY);
		Object nsString = createNSString(key);
		return nsDictionaryClass.getMethod("setValue", idClass, nsStringClass).invoke(dictionary, value, nsString);
	}
	
	private Object getDictionaryValue(String key, Object dictionary)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, InstantiationException, IllegalArgumentException {
		Object id = getIdValue(key, dictionary);
		if (id == null) {
			return null;
		}
		// fixed assumption that returned object is a dictionary
		Class<?> dictionaryClass = Class.forName(CLASS_NAME_NSDICTIONARY);
		Class<?> idClass = Class.forName(CLASS_NAME_ID);
		Constructor<?> constructor = dictionaryClass.getConstructor(idClass);
		if (constructor == null) {
			return null;
		}
		return constructor.newInstance(id);
	}

	private Object getBooleanValue(String key, Object dictionary)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, InstantiationException, IllegalArgumentException {
		Object id = getIdValue(key, dictionary);
		if (id == null) {
			return null;
		}
		// fixed assumption that returned object is a number
		Class<?> numberClass = Class.forName(CLASS_NAME_NSNUMBER);
		Class<?> idClass = Class.forName(CLASS_NAME_ID);
		Constructor<?> constructor = numberClass.getConstructor(idClass);
		if (constructor == null) {
			return null;
		}
		Object value = constructor.newInstance(id);
		return numberClass.getMethod("boolValue").invoke(value);
	}

	private Object getIdValue(String key, Object dictionary)
			throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
		Class<?> nsStringClass = Class.forName(CLASS_NAME_NSSTRING);
		Class<?> nsDictionaryClass = Class.forName(CLASS_NAME_NSDICTIONARY);
		Object nsString = createNSString(key);
		return nsDictionaryClass.getMethod("valueForKey", nsStringClass).invoke(dictionary, nsString);
	}

	private Object createNSString(String value)
			throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {
		Class<?> nsStringClass = Class.forName(CLASS_NAME_NSSTRING);
		Method stringWithMethod = nsStringClass.getMethod("stringWith", String.class);
		return stringWithMethod.invoke(null, value);
	}

	private Object createNSNumber(boolean value) throws ClassNotFoundException, NoSuchMethodException,
			SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Class<?> nsNumberClass = Class.forName(CLASS_NAME_NSNUMBER);
		Method numberWithBoolMethod = nsNumberClass.getDeclaredMethod("numberWithBool", Boolean.TYPE);
		return numberWithBoolMethod.invoke(nsNumberClass, value);
	}
}
