def read_binary_file(file_path):
    with open(file_path, 'rb') as file:
        byte_data = list(file.read())
    return byte_data

def compress_data(byte_data, line_length=16):
    """ Compress data by skipping every other byte in specific lines (two used, two omitted). """
    compressed_data = []
    num_lines = len(byte_data) // line_length
    
    # Process every four lines: take two lines, skip two lines
    for line in range(0, num_lines, 4):  # Increment by 4 to handle blocks of four lines
        for sub_line in range(2):  # Process the first two lines of each block
            if line + sub_line >= num_lines:
                break  # Avoid processing beyond the end of the data
            start = (line + sub_line) * line_length
            end = start + line_length
            current_line = byte_data[start:end]
            
            # Append every other byte from the current line
            compressed_line = current_line[0::2]  # Take every second byte, starting from the first
            compressed_data.extend(compressed_line)
    
    return compressed_data

def write_binary_file(file_path, data):
    with open(file_path, 'w') as file:
        file.write(".byte")
        for byte in range(0, len(data)):
            # if byte % 15 == 0:
            #     file.write(" ${:02X}".format(data[byte]))
            #     file.write("\n.byte ${:02X},".format(data[byte+1]))
            # else: 
            file.write(" ${:02X},".format(data[byte]))

input_file_path = 'screen4.nam'  # Path to background file
output_file_path = 'background4_compress.asm'  # Path to save the compressed data

original_data = read_binary_file(input_file_path)
print(original_data)
compressed_data = compress_data(original_data)

write_binary_file(output_file_path, compressed_data)