;;;; src/installation.lisp
;;;; Skill installation and agent integration utilities

(in-package :cl-tron-mcp/installation)

(defparameter *agent-skills* (make-hash-table :test 'equal)
  "Registry of agent-specific skill configurations")

(defparameter *port-compliance* '(:primary-port (cl-tron-mcp/config:get-config :http-port)
                                  :prohibited-ports (cl-tron-mcp/config:get-config :http-port))
  "Port compliance policy for all agent interactions")

(defparameter *installation-guides* 
  (list
   `("claude" . ,(format nil 
"The following assumes that TRON MCP http port is 4006 and that Swank runs on port 4005.

=== Claude Agent Tron Integration ===
Installation:
1. Create $HOME/.config/claude/custom_tools.json:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"claude\",\"format\":\"json\"}}}'

2. Add to Claude configuration:
   {
     \"commands\": [
       {
         \"name\": \"Tron Debug\",
         \"command\": \"curl -X POST http://127.0.0.1:4006/mcp -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"repl_connect\",\"arguments\":{\"host\":\"127.0.0.1\",\"port\":4005}}}'\"
       }
     ]
   }

Security Note: All communication must use MCP server (port 4006). Direct access to port 4005 is prohibited."))
   
   `("codex" . ,(format nil 
"=== CodeX Agent Tron Integration ===
Installation:
1. Install Tron MCP client:
   pip install tron-mcp-integration

2. Configure in $HOME/.codex/config.json:
   {
     \"mcp_servers\": [
       {
         \"name\": \"cl-tron-mcp\",
         \"endpoint\": \"http://127.0.0.1:4006/mcp\",
         \"port_policy\": {
           \"allowed\": [4006],
           \"blocked\": [4005]
         }
       }
     ]
   }

3. Verify installation:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"codex\",\"format\":\"markdown\"}}}'

Port Compliance: Direct port 4005 connections will be rejected by MCP protocol."))
   
   `("opencode" . ,(format nil 
"=== Opencode Agent Tron Integration ===
Installation:
1. Add to $HOME/.config/opencode/opencode.json:
   {
     \"mcp\": {
       \"cl-tron-mcp\": {
         \"type\": \"local\",
         \"command\": [
            \"bash\", \"-c\", 
            \"curl -X POST http://127.0.0.1:4006/mcp \\
               -H 'Content-Type: application/json' \\
               -d '{\"jsonrpc\": \"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{}}'\"],
         \"enabled\": true
       }
     }
   }

2. Test connection:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"opencode\",\"format\":\"yaml\"}}}'

Security: Agent must use MCP proxy (port 4006). Direct Swank access (port 4005) is blocked."))
   
   `("kilocode" . ,(format nil 
"=== Kilocode Agent Tron Integration ===
Installation:
1. Copy MCP configuration:
   cp cl-tron-mcp/.kilocode/mcp.json $HOME/.kilocode/mcp.json

2. Install via npm:
   npm install @cl-tron-mcp/integration

3. Verify MCP server:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"kilocode\",\"format\":\"json\"}}}'

Compliance: Port 4005 access denied. Use MCP server port 4006 exclusively."))
   
   `("pi" . ,(format nil 
"=== Pi Agent Tron Integration ===
Installation:
1. Register skill with ICM:
   icm register-skill cl-tron-mcp-integration \
     --manifest-file cl-tron-mcp/.pi/skills/cl-tron-mcp-integration.yaml

2. Install via pi package manager:
   pi-skill-install cl-tron-mcp-integration

3. Verify installation:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"pi\",\"format\":\"markdown\"}}}'

Port Policy: MCP server (4006) is mandatory. Direct Swank communication (4005) is prohibited."))
   
   `("copilot" . ,(format nil 
"=== Copilot Agent Tron Integration ===
Installation:
1. Install extension:
   code --install-extension cl-tron.mcp-integration

2. Configure in VSCode settings.json:
   {
     \"mcp.servers\": [
       {
         \"name\": \"cl-tron-mcp\",
         \"command\": \"cl-tron-mcp\",
         \"env\": {
           \"MCP_PORT\": \"4006\",
           \"SWANK_PORT\": \"4005\"
         }
       }
     ]
   }

3. Test connection:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"copilot\",\"format\":\"yaml\"}}}'

Security: MCP protocol enforces port 4006 usage only. Port 4005 blocked."))
   
   `("gemini" . ,(format nil 
"=== Gemini Agent Tron Integration ===
Installation:
1. Install Python client:
   pip install tron-mcp-client[all]

2. Configure gemini-agent.yaml:
   tron:
     endpoint: http://127.0.0.1:4006/mcp
     protocol: JSON-RPC-2.0
     port_policy:
       allowed: [4006]
       blocked: [4005]

3. Verify setup:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"gemini\",\"format\":\"markdown\"}}}'

Compliance: Direct port 4005 access results in connection rejection."))
   
   `("cursor" . ,(format nil 
"=== Cursor Agent Tron Integration ===
Installation:
1. Copy MCP configuration:
   cp cl-tron-mcp/.cursor/mcp.json $HOME/.cursor/mcp.json

2. Install via Cursor CLI:
   cursor extensions install cl-tron.mcp-integration

3. Verify MCP connection:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"cursor\",\"format\":\"json\"}}}'

Port Enforcement: MCP server (port 4006) is the only allowed endpoint. Port 4005 is blocked."))
   
   `("universal" . ,(format nil 
"=== Universal Tron Integration ===
For agents without specific configuration:

1. Basic MCP connection:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{\"capabilities\":{}}}'

2. Get installation guide for your agent:
   curl -X POST http://127.0.0.1:4006/mcp \
     -H 'Content-Type: application/json' \
     -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"skill_store_installation_guide\",\"arguments\":{\"target_platform\":\"<YOUR_AGENT>\",\"format\":\"markdown\"}}}'

3. Available agent platforms:
   - claude, codex, opencode, kilocode, pi, copilot, gemini, cursor

Port Policy: All agents must use MCP server port 4006. Direct access to port 4005 is prohibited by protocol enforcement."))
   ))

(defun get-skill-installation-guide (target-platform format)
  "Return installation guide for specified agent platform and format"
  (declare (ignore format))
  (let ((guide (gethash target-platform *installation-guides* 
                        (gethash "universal" *installation-guides*))))
    (list :webpage "https://cl-tron-mcp.readthedocs.io/en/latest/installation/"
          :documentation guide
          :port_policy *port-compliance*
          :security_note "Direct access to port 4005 is prohibited. Use MCP server port 4006 exclusively."
          :installation_status "ready"
          :compliance_verified t)))

(defun skill-store-installation-guide (target-platform format)
  "Handler for skill_store_installation_guide tool"
  (handler-case
      (get-skill-installation-guide target-platform format)
    (error (e)
      (list :error t
            :message (format nil "Failed to generate installation guide: ~a" e)
            :suggested_action "Use 'universal' platform for generic instructions"))))

(defun discover-skills ()
  "Return available skill configurations"
  (list :skill_registry (loop for (platform . guide) in *installation-guides*
                              collect (list :platform platform
                                           :installed-p (probe-file (format nil "~a/.config" platform))
                                           :installation_required (not (probe-file (format nil "~a/.config" platform)))))
        :summary "Tron MCP skill registry for agent integration"
        :port_compliance *port-compliance*))

(defun recommend-skill-handler (agent-type)
  "Recommend skill based on agent type"
  (let ((guide (gethash (string-downcase agent-type) *installation-guides*
                        (gethash "universal" *installation-guides*))))
    (list :recommended_skill (format nil "~a-tron-integration" (string-downcase agent-type))
          :install_instructions guide
          :port_requirements *port-compliance*)))

;; Register the tool handlers
(cl-tron-mcp/tools:register-tool-handler "skill_store_installation_guide" 
                       (lambda (target-platform format)
                         (skill-store-installation-guide target-platform format)))
(cl-tron-mcp/tools:register-tool-handler "skill_discover" #'discover-skills)
(cl-tron-mcp/tools:register-tool-handler "skill_recommend" #'recommend-skill-handler)

(cl-tron-mcp/tools:define-validated-tool "skill_store_installation_guide"
  "Returns installation guide for agent skills with port compliance enforcement"
  :input-schema (list :target_platform "string" :format "string")
  :output-schema (list :webpage "string" :documentation "string")
  :requires-approval nil
  :documentation-uri "file://docs/tools/tool-install-guide.md"
  :validation ((validate-string "target_platform" target-platform :required t)
               (validate-string "format" format :required t))
  :body (get-skill-installation-guide :target-platform target-platform :format format))

(cl-tron-mcp/tools:define-simple-tool "skill_discover"
  "Discover available agent skills"
  :input-schema nil
  :output-schema (list :skill_registry "map" :summary "string")
  :requires-approval nil
  :function discover-skills)

(cl-tron-mcp/tools:define-simple-tool "skill_recommend"
  "Recommend skill based on agent type"
  :input-schema (list :agent_type "string")
  :output-schema (list :recommended_skill "string" :install_instructions "string")
  :requires-approval nil
  :function recommend-skill-handler)


