/*******************************************************************************
 * Copyright (c) 2010 JVM Monitor project. All rights reserved. 
 * 
 * This code is distributed under the terms of the Eclipse Public License v1.0
 * which is available at http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.jboss.tools.common.jdt.debug.tools.internal;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

/**
 * The constants.
 */
@SuppressWarnings("nls")
public interface IToolsConstants {
    /** The qualified class name for HotSpotVirtualMachine. */
    static final String HOT_SPOT_VIRTUAL_MACHINE_CLASS = "sun.tools.attach.HotSpotVirtualMachine";

    /** The method name for HotSpotVirtualMachine#heapHisto(). */
    static final String HEAP_HISTO_METHOD = "heapHisto";

    /** The qualified class name for MonitoredVm. */
    static final String MONITORED_VM_CLASS = "sun.jvmstat.monitor.MonitoredVm";
    
    static final String MONITORED_VM_UTIL_CLASS = "sun.jvmstat.monitor.MonitoredVmUtil";
    
    /** The method name for MonitoredVm#findByName(). */
    static final String FIND_BY_NAME_METHOD = "findByName";

    /** The qualified class name for Monitor. */
    static final String MONITOR_CLASS = "sun.jvmstat.monitor.Monitor";

    /** The method name for Monitor#getValue(). */
    static final String GET_VALUE_METHOD = "getValue";

    /** The qualified class name for VmIdentifier. */
    static final String VM_IDENTIFIER_CLASS = "sun.jvmstat.monitor.VmIdentifier";

    /** The qualified class name for MonitoredHost. */
    static final String MONITORED_HOST_CLASS = "sun.jvmstat.monitor.MonitoredHost";

    /** The method name for MonitoredHost#getMonitoredVm(). */
    static final String GET_MONITORED_VM_METHOD = "getMonitoredVm";

    /** The method name for MonitoredHost#activeVms(). */
    static final String ACTIVE_VMS_METHOD = "activeVms";

    /** The qualified class name for VirtualMachine. */
    static final String VIRTUAL_MACHINE_CLASS = "com.sun.tools.attach.VirtualMachine";

    /** The method name for VirtualMachine#loadAgent(). */
    static final String LOAD_AGENT_METHOD = "loadAgent";

    /** The method name for VirtualMachine#loadAgent(). */
    static final String START_LOCAL_MANAGEMENT_AGE_METHOD = "startLocalManagementAgent";

    /** The method name for VirtualMachine#detach(). */
    static final String DETACH_METHOD = "detach";

    /** The method name for VirtualMachine#getSystemProperties(). */
    static final String GET_SYSTEM_PROPERTIES_METHOD = "getSystemProperties";

    /** The method name for VirtualMachine#getMonitoredHost(). */
    static final String GET_MONITORED_HOST_CLASS = "getMonitoredHost";

    /** The method name for VirtualMachine#getAgentProperties(). */
    static final String GET_AGENT_PROPERTIES_METHOD = "getAgentProperties";

    /** The method name for URLClassLoader#addURL(). */
    static final String ADD_URL_METHOD = "addURL";

    /** The method name for VirtualMachine#attach(). */
    static final String ATTACH_METHOD = "attach";

    /** The library name for attach. */
    static final String ATTACH_LIBRARY = "attach";

    /** The JRE library paths. */
    static final String[] LIBRARY_PATHS = {
            // windows
            File.separator + "jre" + File.separator + "bin",
            // linux 32bit
            File.separator + "jre" + File.separator + "lib" + File.separator
                    + "i386",
            // linux 64bit
            File.separator + "jre" + File.separator + "lib" + File.separator
                    + "amd64",
            // mac
            File.separator + "jre" + File.separator + "lib" };

    /** The relative path from JDK root directory to tools.jar. */
    static final String TOOLS_JAR = File.separator + "lib" + File.separator
            + "tools.jar";

    /** The Java installation directory on Mac. */
    static final String JAVA_INSTALLATION_DIR_ON_MAC = "JavaVirtualMachines";

    /** The option for heap histogram to get all objects. */
    static final String HEAP_HISTO_ALL_OPTION = "-all";

    /** The option for heap histogram to get only live objects. */
    static final String HEAP_HISTO_LIVE_OPTION = "-live";


    /** The VM identifier. */
    static final String VM_IDENTIFIRER = "//%d?mode=r";

    /** The key for Java command. */
    static final String JAVA_COMMAND_KEY = "sun.rt.javaCommand";


    /** The system property key for Java home. */
    static final String JAVA_HOME_PROPERTY_KEY = "java.home";
}
